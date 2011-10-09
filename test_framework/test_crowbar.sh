#!/bin/bash
# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 

# This script expects to be able to run certian commands as root.
# Either run it as a user who can sudo to root, or give the user
# you are running it as the following sudo rights:
# crowbar-tester ALL = NOPASSWD: /bin/mount, /bin/umount, /bin/ip, /usr/sbin/brctl, /home/crowbar-tester/test_framework/make-cgroups.sh

# We expect to live in $HOME/test_framework.
# We use bash4 specific functionality (hash tables), and cgroups to
# make sure we clean up everything when we exit a test run.
# You will need a fairly recent Linux distro to run this test -- 
# RHEL/CentOS 5 will not work without some significant rework.

# always run in debugging mode, and log everything to a test log
exec 2>"$HOME/smoketest.log"
export DEBUG=true
export PS4='(${nodename:-none})${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): ';
export CROWBAR_KEY="crowbar:crowbar"
set -x

# die horribly if any simple command fails.
set -e

# Lockfiles to ensure that we don't accidentally end up 
# stepping on our toes.

# This lock is held whenever we are copying an crowbar .ISO
# around to get it ready for testing.
UNTESTED_LOCK="$HOME/.untested.lck"

# THis lock is held whenever we are running tests.  It exists to
# prevent multiple instances of the smoketest from running at once.
TESTING_LOCK="$HOME/.testing.lck"

# This lock is held whenever we are starting or killing KVM.
# We use it to ensure that only one virtual machine is being set
# up or torn down at any given time, and that our teardown 
# function does not race with our launching function.
KVM_LOCK="$HOME/.kvm.lck"

# This lock is held whenever we are performing a cleanup.  
# We want to ensure that we never try to run two
# cleanups in parallel.
CLEANUP_LOCK="$HOME/.cleanup.lck"

FRAMEWORKDIR="$HOME/test_framework"

# We run all KVM instances in a screen session.
SCREENNAME="crowbar-test"

# The number of computer nodes we will end up deploying.
# Each node has 2 gigs of memory, 2 cores, and a 6 gig disk 
# image, so make sure your test machine can handle the load.
VIRT_NODES=(virt-1 virt-2 virt-3 virt-4)

# Number of seconds we wait for compute nodes to go through a full deploy.
COMPUTE_DEPLOY_WAIT=2400

# IP address of the node that ends up as the admin node.
export CROWBAR_IP=192.168.124.10

# The names of the bridges we will create.  Bridge names must be
# 15 characters or less due to kernel name constraints.
# The last part of the name will be used in tap interface name generation.
# Please keep it at 4 characters or less.
BRIDGES=(crowbar-pub crowbar-priv)

# An array of physical interfaces and the bridges they should be bound to.
# We need to use real physical interfaces becasue Crowbar assumes
# it can create and destroy vlans as needed.
# Each entry in this array is if the form ifname,bridgename
# PHYSICAL_INTERFACES=(eth1,crowbar-pub)

# An array of MAC addresses of the primary interfaces of the physical machines.
# We need to have this information beforehand so that we can send 
# Wake On LAN packets to them.
PHYSICAL_MACS=()

# Source a local config file.  Settings in it will override the 
# ones here.

if [[ -f $HOME/.test-crowbar ]]; then
    . "$HOME/.test-crowbar"
fi

# A hash containing slave name -> hostname mappings.
declare -A SLAVES

# Does exactly what it says.
# $1 = status to exit with
# $2 = error message to print.
die() { local _res=$1; shift; echo "$*"; exit $_res; }

grep -q cgroup /proc/filesystems || die 1 "We must have cgroups to track our processes for proper cleanup!"

update_status() {
    # $1 = VM status to update.
    # $2 = Status update
    local current_date=$(date '+%F %T %z')
    echo "$current_date: $1 - $2"
    {
	flock 66
	echo "$current_date: $2" >> "$testdir/$1.status"
    } 66>"$testdir/.$1.status.lck"
}

# make the bridges we will use for testing.
make_bridges() {
    local pub_re='pub$'
    local bridge
    for bridge in "${BRIDGES[@]}"; do
	sudo brctl show |grep -q "$bridge" || \
	    sudo brctl addbr "$bridge" || \
	    die -1 "Could not create $bridge bridge!"
	sudo brctl setfd "$bridge" 1 || \
	    die -3 "Could not set forwarding time on $bridge!"
	sudo ip link set "$bridge" up || \
	    die -2 "Could not set link on $bridge up!"
	sudo brctl stp "$bridge" on
	if [[ $bridge =~ $pub_re ]]; then
	    sudo ip addr add 192.168.124.1/24 dev "$bridge"
	fi
    done
    # Bind the physical nics we want to use to the appropriate bridges.
    for iface in "${PHYSICAL_INTERFACES[@]}"; do
	local ifname=${iface%%,*}
	local bridge=${iface##*,}
	sudo ip link set "$ifname" up
	sudo brctl addif "$bridge" "$ifname"
    done
}

# remove our bridges after we are done with them.
kill_bridges() {
    # Ignore any simple errors in this function.
    set +e
    # Unbind any physical nics we have bound.
    for iface in "${PHYSICAL_INTERFACES[@]}"; do
	local ifname=${iface%%,*}
	local bridge=${iface##*,}
	sudo ip link set "$ifname" down
	sudo brctl delif "$bridge" "$ifname"
    done
    # Tear down the bridges we created.
    for bridge in "${BRIDGES[@]}"; do
	sudo ip link set "$bridge" down
	sudo brctl delbr "$bridge"
    done
}

get_cluster_logs() (
    set +e
    echo "$(date '+%F %T %z'): Gathering $1 logs, please wait."
    cd "$LOGDIR"
    [[ -f $testdir/admin.pid ]] || return 1
    local curlargs=(-L -o "$1-$(date '+%Y%m%d-%H%M%S').tar" \
	--connect-timeout 120 --max-time 120)
    [[ $CROWBAR_KEY ]] && curlargs+=(-u "$CROWBAR_KEY" --digest) || :
    curl "${curlargs[@]}" "http://$CROWBAR_IP:3000/support/logs"
    echo "$(date '+%F %T %z'): Done gathering $1 logs."
)

# Tidy up after ourselves when we exit.
# This take care to make sure we don't have any stray
# child processes left behind (including VMs), tear down any 
# network infrastructure we created, and create any success
# or failure logs we need to create.  
declare -a cleanup_cmds taps
cleanup() {
    # We ignore errors in this function.
    set +e
    {
	# Make sure only one instance of cleanup is running 
	# at any given time.
	flock 70
	# Once we have started running, make sure we are not invoked again.
	trap 0
	killall check_ready
	[[ $develop_mode = true ]] && pause
	# Gather final logs if our admin node is up.
	get_cluster_logs final
	# Make sure our virtual machines have been torn down.
	for pidfile in "$testdir/"*.pid; do
	    local vmname=${pidfile##*/}
	    vmname=${vmname%.pid}
	    kill_vm "$vmname" || :
	done
	# If there are any commands to run at cleanup, run them now.
	for c in "${cleanup_cmds[@]}"; do
	    eval $c || :
	done
	# If our .iso is still mounted, umount it.
	sudo /bin/umount -d "$LOOPDIR" &>/dev/null
	# Tear down out network.
	kill_virt_net &>/dev/null
	# Kill our screen session
	screen -S "$SCREENNAME" -X quit &>/dev/null
	# Kill anything else we make have left behind.
	for task in $(cat "$CGROUP_DIR/tasks"); do
	    [[ $task = $$ || $task = $SMOKETEST_PID ]] && continue
	    kill -9 $task
	done
	# Make sure we exit with the right status code.
	[[ $final_status ]] || exit 1
	# Copy passed and failed logs to the right location.
	[[ $test_results ]] && {
	    for result in "${test_results[@]}"; do
		echo "$result"
		[[ $result = *Failed ]] && final_status=Failed
	    done
	}
	echo "Deploy $final_status."
	target="$HOME/tested/${testdir##*/}-$(date '+%Y%m%d-%H%M%S')-${final_status}"
	rm -f "$testdir/"*.disk || :
	mv "$testdir" "$target"
	cp "$HOME/smoketest.log" "$target"
	(cd "$HOME/tested"; tar czf "$target.tar.gz" "${target##*/}")
	echo "Logs are available at $target.tar.gz."
	echo "(on the Web at http://10.9.244.31:3389/crowbar-test/${target##*/}/)" 
	rm "$HOME/tested/"*.iso
	if [[ $final_status != Passed ]]; then
	    ret=1
	else
	    ret=0
	fi
    } 70>"$CLEANUP_LOCK"
    exit $ret
}

# Simple unique mac address generation.
# This will not work if you end up spinning up more than 65535 vm interfaces.
MACNR=1
getmac() {
    MACADDR=$(printf "52:54:%02x:12:34:%02x" $((MACNR/256)) $((MACNR%256)))
    ((MACNR++))
    :
}

# Make a tap interface, and attach it to the right bridge.
maketap() {
    # $1 = preferred device name
    # $2 = bridge to attach it to.

    # preemptively arrange to clean up.
    sudo ip tuntap add dev "$1" mode tap || \
	die -1 "Could not create tap device $1"
    sudo ip link set "$1" up || \
	die -2 "Could not bring link on tap device $1 up!"
    sudo brctl addif "$2" "$1" || \
	die -3 "Could not add tap $1 to bridge $2!"
}

# Remove a tap interface we created.
killtap() {
    set +e
    local res_re='(does not exist|Cannot find device)'
    # $1 = device to kill
    # $2 = bridge to detach it from
    while ! [[ $(sudo ip link show "$1" 2>&1) =~ $res_re ]]; do 
	sudo brctl delif "$2" "$1"
	sudo ip link set "$1" down
	sudo ip tuntap del dev "$1" mode tap
    done
}

# Build up our local virtual net infrastructure.
make_virt_net() {
    make_bridges
    local node bridge
    for node in admin ${VIRT_NODES[@]}; do
	for bridge in "${BRIDGES[@]}"; do
	    local nic_name="${node}-${bridge##*-}"
	    maketap "$nic_name" "$bridge"
	done
    done
}

# Tear down our local virtual net infrastructure.
kill_virt_net() {
    set +e
    local node bridge
    for node in admin ${VIRT_NODES[@]}; do
	for bridge in ${BRIDGES[@]}; do
	    local nic_name="${node}-${bridge##*-}"
	    killtap "$nic_name" "$bridge"
	done
    done
    kill_bridges
}

# Make MAC addresses for the network interfaces for a VM. 
makenics() {
    # $1 = base name for each nic. Must be 9 characters or less.
    vm_nics=()
    local node bridge
    for bridge in ${BRIDGES[@]}; do
	local nic_name="$1-${bridge##*-}"
	getmac
	if [[ $nic_name =~ virt-.-pub ]]; then
	    SLAVES["$1"]="d${MACADDR//:/-}.pod.cloud.openstack.org"
	fi
	vm_nics+=("$MACADDR,$nic_name")
    done
}

# find the newest file. Returns it as $NEWEST_FILE
find_newest_file() {
    # $@ = files to discriminate against
    local age newest_age f
    unset NEWEST_FILE
    for f in "$@"; do
	[[ -f $f ]] || continue
	age="$(date -r "$f" '+%s')"
	if ((age > newest_age)); then
	    newest_age=$age
	    NEWEST_FILE=$f
	fi
    done
    [[ $NEWEST_FILE ]] && return 0 || return 1
}

# grab the latest iso from untested and populate testing with it.
# This should be called with the testing lock held, and it will
# grab the untested lock.
find_latest_iso() {
    flock -x 200
    test_isos=("$HOME/crowbar"*.iso)
    find_newest_file "${test_isos[@]}" || return 1 # nothing to do.
    # If we have a sumfile, hack it up to point at the "right" .iso
    # and check to see if our .iso checks out.
    for sumfile in "${NEWEST_FILE}".*sum; do
	[[ -f $sumfile ]] || continue
	location=${sumfile%/*}
	sumtype=${sumfile##*.}
	sed -i "s@iso_library@${location}@" "$sumfile"
	if ! "$sumtype" -c --status "$sumfile" &>/dev/null; then
	    die 1 "${NEWEST_FILE} corrupt, does not match ${sumfile}."
	fi
    done
    # Delete any stale .*sum files.
    find "$HOME" -name '*.iso.*sum' -delete &>/dev/null || :

    for iso in "${test_isos[@]}"; do
	if [[ -f $iso && -f $NEWEST_FILE && \
	    $iso != $NEWEST_FILE && $iso -ot $NEWEST_FILE ]]; then rm "$iso"
	else
	    NEWEST_FILE="$iso"
	fi
    done
    # This is the newest file.  
    # Make sure we have the whole thing.    
    this_stat=$(stat "$NEWEST_FILE")
    while [[ $last_stat != $this_stat ]]; do
	sleep 1
	last_stat="$this_stat"
	this_stat=$(stat "$NEWEST_FILE")
    done
    echo "$NEWEST_FILE"
} 200>"$UNTESTED_LOCK"

# Kill a running VM.
kill_vm() (
    flock 65
    # $1 = vmname
    # $2 = state to assign, defaults to killed.
    set +e
    local killsig=TERM killtries=0 killstate="${2:-killed}"
    # If there is no PID file, assume that the VM is already dead.
    [[ -f $testdir/$1.pid ]] || {
	> "$testdir/$1.$killstate"
	flock -u 65
	return 0
    }
    local pid=$(cat "$testdir/$1.pid")
    # If there is no entry for the VM process in /proc/ assume the VM is dead.
    # If there is, cd into that directory so that we can be sure 
    # we are killing the right process and so that we can tell when the process
    # is dead -- when the process has finished exiting, the kernel will
    # unlink /proc/$pid and everything in it.  Even if the kernel manages 
    # to cycle through pidspace to create another process with the same pid 
    # between kill attempts, we will not see entry for that process in 
    # our $PWD, because it will be a different dentry and set of inodes that
    # happen to have the same name.
    cd "/proc/$pid" &>/dev/null || {
	rm -f "$testdir/$1.pid" &>/dev/null
	> "$testdir/$1.$killstate"
	flock -u 65
	return 0
    }
    # If the cmdline entry does not exist in /proc/$pid, the process is
    # already dead but the kernel has not finished cleaning up.
    [[ -f cmdline ]] || {
	rm -f "$testdir/$1.pid" &>/dev/null
	> "$testdir/$1.$killstate"
	flock -u 65
	return 0
    }
    # If the /proc/$pid/cmdline does not contain the name of our VM,
    # the kernel has finished cleaning up our process and enough processes
    # have been spawned that our PID has been reused.
    grep -q "$1" cmdline || {
	rm -f "$testdir/$1.pid" &>/dev/null
	> "$testdir/$1.$killstate"
	flock -u 65
	return 0
    }
    # Loop trying to kill this VM.  Escalate our violence and sleep longer
    # the more tries we take.
    while (( killtries++ < 10)); do
	update_status $vmname "Killing $vm_gen (try $killtries, signal $killsig)"
	kill "-$killsig" "$pid"
	((killtries < 5)) || killsig=KILL
	sleep $killtries
	# if /proc/$pid/cmdline (or any other file that normally exists there)
	# is gone, the process is dead, and our work is done.
	if [[ ! -f cmdline ]]; then
	    update_status $vmname "Killed with SIG${killsig}"
	    rm -f "$testdir/$vmname.pid" &>/dev/null
	    > "$testdir/$vmname.$killstate"
	    flock -u 65
	    return 0
	fi
    done
    flock -u 65
    die -1 "Could not kill $vmname, something went horribly wrong."
) 65>"$KVM_LOCK"

# Wait for a KVM instance to die naturally, for a timeout to expire,
# or for a daemonization condition to be reached.
wait_for_kvm() {
    # $1 = name of KVM instance.
    local vmname=$1
    shift
    local pidfile="$testdir/$vmname.pid"
    [[ -f $pidfile ]] || return 1 # no pidfile? Bad Things happened.
    local kvmpid=$(cat "$pidfile")
    while [[ $1 ]]; do
	case $1 in
	    -timeout) local deadline=$(($(date +%s) + $2)) timeout=$2; shift;;
	    -daemonif) local daemonif=$2; shift;;
	    -dieif) local dieif=$2; shift;;
	    *) break;;
	esac
	shift
    done
    local lastres= thisres=
    # Use the same /proc/$kvmpid trick we used in kill_vm to 
    # make sure we are watching the right process.
    (   cd "/proc/$kvmpid"
	# if /proc/$kvmpid/$cmdline does not contain the name of our
	# VM, something went horrbly wrong.
	[[ $(cat cmdline) =~ $vmname ]] || return 1
	while [[ -f cmdline ]]; do
	    # If there is a condition on which we should kill the VM
	    # immediatly, test and see if it is true.
	    if [[ $dieif ]] && $dieif; then
		update_status "$vmname" "Ran into instant-kill condition."
		return 1
	    fi
	    # If there is a condition on which we should stop waiting for
	    # a VM, test to see if it is true.
	    if [[ $daemonif ]]; then
		# We assign the output of $daemonif to a variable so that
		# we don't spam up the test run transcript.
		if thisres=$($daemonif); then
		    # If it is, stop watching this VM.
		    update_status "$vmname" "$thisres"
		    update_status "$vmname" \
			"Daemonizing node with $(($deadline - $(date +%s))) seconds left."
		    return 0
		elif [[ $thisres =~ problem && $vmname =~ admin && \
		    ! $develop_mode ]]; then
		    update_status "$vmname" "$thisres"
		    update_status "$vmname" "Transition to problem state not allowed"
		    return 1
		elif [[ $thisres && $lastres != $thisres ]]; then
		    update_status "$vmname" "$thisres"
		    lastres="$thisres"
		fi
	    fi
	    # If we were supposed to test for a deadline and we overran it,
	    # return with the appropriate status code.
	    if [[ $deadline && ! $develop_mode ]] && (($(date +%s) > $deadline)); then
		update_status "$vmname" "Node ran for more than $timeout seconds."
		return 1
	    fi
	    sleep 10
	done
	# If we wanted to be daemonized but were not, game over man.
	if [[ $daemonif ]]; then
	    update_status "$vmname" "Node failed to daemonize."
	    return 1
	else
	    # We appear to have exited normally.
	    update_status "$vmname" "Node exited."
	    return 0
	fi
    )
}

# Hash that allows us to track the number of reboots a VM has had.
declare -A kvm_generations

# Hack to pick the fastest disk caching mode.
# We use unsafe caching if we can on the vms because we will just
# rebuild the filesystems from scratch if anything goes wrong.
if kvm --help |grep -q 'cache.*unsafe'; then
    drive_cache=unsafe
else
    drive_cache=writeback
fi

# Run a KVM session.
run_kvm() {
    # run_kvm will try to process the arguemnts it knows about first.
    # It will assume that the first argument it does not know how to process
    # is the name of the VM you want to create, and that any remining arguments
    # are to be passed to KVM verbatim.
    # Note that you must provide either a -bootc or a -bootn argument
    # before the name of the VM if you want the VM to boot off the first
    # hard drive or to PXE boot.  If you do not, or do not otherwise arrange
    # for a way to boot the VM, it will never boot.  The reason for this
    # is the design decision that anu hard drives not attached to IDE channels
    # are not considered boot candidates without a special argument passed
    # to the -drive parameter, and we have to use SCSI attached hard drives to 
    # work around device naming differences in CentOS 5 (for Sledgehammer) and
    # more current kernels.
    # $1 = name of KVM to run. Disk image and logfile names are
    # derived from this.
    # KVMs are always oneshot virtual machines -- this works around 
    # a few booting and rebooting bugs.
    # $@ = after shifing, other args to be passed to kvm.
    # In addition, we expect that the caller has set vm_nics appropriatly.
    local waitargs=() reboot=false 
    local pxeboot='' driveboot=''
    while true; do
	case $1 in
	    # -daemon tells the framework to stop active monitoring
	    # once the PID file is written.
	    -daemon) waitargs+=('-daemonif' 'true');;
	    # -bootc tells the VM to boot off the first hard drive
	    -bootc) driveboot=true;;
	    # -bootn tells the VM to PXE boot.
	    -bootn) pxeboot=true;;
	    # -timeout will wait up to $2 seconds before killing the VM if 
	    # we are actively monitoring the VM.
	    -timeout) waitargs+=("$1" "$2"); shift;;
	    # -daemonif will have the framework stop actively monitoring the 
	    # VM once $2 exits with a zero status.
	    -daemonif) waitargs+=("$1" "$2"); shift;;
	    -dieif) waitargs+=("$1" "$2"); shift;;
	    # -reboot allows the VM to reboot instead of halting on reboot.
	    -reboot) reboot=true;;
	    *) break;;
	esac
	shift
    done
    local vmname=$1
    shift
    # Track the number of times that this VM has been started by the 
    # framework.
    if [[ ! ${kvm_generations["$vmname"]} ]]; then
	kvm_generations["$vmname"]=1
    else
	kvm_generations["$vmname"]=$((${kvm_generations["$vmname"]} + 1))
    fi
    local cpu_count=2 mem_size=2G
    if [[ $vmname = admin ]] ; then
      cpu_count=4
      mem_size=4G
    fi
    local drivestr="file=$testdir/$vmname.disk,if=scsi,format=raw,cache=$drive_cache"
    if [[ $driveboot ]]; then
	drivestr+=",boot=on"
    fi
    local vm_gen="$vmname.${kvm_generations[$vmname]}"
    # create a new log directory for us.  vm_logdir needs to be global
    # because other things will need to read the logs to see what is happening.
    vm_logdir="$testdir/logs/$vm_gen"
    mkdir -p "$vm_logdir"
    local pidfile="$testdir/$vmname.pid"
    [[ -f $pidfile ]] && rm "$pidfile" || :
    # Our initial array of args for kvm.
    # We pass in our own ROM for PXE booting to work around some old
    # PXE boot bugs in older versions of the E1000 etherboot roms.
    local kvmargs=(-enable-kvm 
	-m $mem_size
	-smp $cpu_count
	-drive "$drivestr"
	-pidfile "$pidfile"
	-serial "file:$vm_logdir/ttyS0.log"
	-serial "file:$vm_logdir/ttyS1.log"
	-name "kvm-$vm_gen")
    # Add appropriate nics based on the contents of vm_nics.
    for line in "${vm_nics[@]}"; do
	kvmargs+=(-net "nic,macaddr=${line%%,*},model=e1000")
	kvmargs+=(-net "tap,ifname=${line##*,},script=no,downscript=no")
    done
    if [[ $pxeboot ]]; then
	kvmargs+=(-boot "order=n" -option-rom "$FRAMEWORKDIR/8086100e.rom")
    elif [[ $driveboot ]]; then
	kvmargs+=(-boot "order=c")
    fi
    # Add additional disks if we have any.
    for image in "$testdir/$vmname-"*".disk"; do
	[[ -f $image ]] || continue
	kvmargs+=(-drive "file=$image,if=scsi,format=qcow2,cache=$drive_cache")
    done

    if [[ $reboot = false ]]; then
	kvmargs+=(-no-reboot)
    fi
    {
	# Make sure that we serialize KVM launches.
 	# Sometimes we get a boot failure if we try launching
	# KVM instances in parallel. As long as we wait for
	# the pidfile to be created, we should be OK -- kvm
	# does not create the pidfile until it has finshed launching
	# the VM.
	flock 65
	# If we are running under X, then use a graphical display.
	if [[ $DISPLAY ]]; then
	    kvmargs+=( -sdl -daemonize )
	    kvm "${kvmargs[@]}" "$@"
	else
	    # otherwise, launch ourselves under screen.
	    kvmargs+=( -curses )
	    screen -S "$SCREENNAME" -X screen \
		-t "$vm_gen" kvm "${kvmargs[@]}" "$@"
	    screen -S "$SCREENNAME" -p "$vm_gen" -X log on
	fi
        # wait up to 10 seconds for a PID file
	local s
	for ((s=0; s<10; s++)); do
	    [[ -f $pidfile ]] && break
	    sleep 1
	done
	flock -u 65
    } 65>"$KVM_LOCK"
    # Now that we have launched the KVM instance, wait for it using the
    # waitargs we were passed.
    wait_for_kvm "$vmname" "${waitargs[@]}" || return 1
}

# This expects to be run with the testing lock held.
# It boots the admin node from the .iso in the testing directory, 
# injecting the appropriate kernel parameters to make things
# work in the test environment.
run_admin_node() {
    # mount our .iso file as a loopback device.  Since the default
    # setup tries to got straight to graphics mode and we want to
    # test everything using a serial console, some Special Help
    # is needed

    # First, set up our admin node.
    local nodename="admin"
    local vm_nics
    makenics admin

    update_status admin "Mounting .iso"
    sudo /bin/mount -o loop "$ISO" "$LOOPDIR" &>/dev/null || \
	die 1 "Could not loopback mount $ISO on $LOOPDIR." 
    cleanup_cmds+=("sudo /bin/umount -d '$LOOPDIR'") 
    
    update_status admin "Hacking up kernel parameters"
    # OK, now figure out what we need to grab by reading the
    # isolinux.cfg file.
    kernel_re='kernel (.+)$'
    append_re='append([^a-zA-Z/=?]+)(.*)$'
    initrd_re='initrd=([^ ]+)'
    console_re='console=([^ ]+)'
    unset kernel kernel_params initrd
    
    while read line; do
	[[ ! $kernel && ( $line =~ $kernel_re ) ]] && \
	    kernel="${BASH_REMATCH[1]}" || :
	[[ ! $kernel_params && ( $line =~ $append_re ) ]] && \
	    kernel_params=${BASH_REMATCH[2]} || :
	[[ ! $initrd && $kernel_params && ( $kernel_params =~ $initrd_re ) ]] && {
	    kernel_params=${kernel_params/append=${BASH_REMATCH[1]}/}
	    initrd="${BASH_REMATCH[1]}"
	} || :
    done < "$LOOPDIR/isolinux/isolinux.cfg"

    # Fix up our paths to the initrd and the kernel
    if [[ -d "$LOOPDIR/Server" ]]; then
        # RHEL keeps its kernel and initrd in isolinux
	kernel="$LOOPDIR/isolinux/$kernel"
	initrd="$LOOPDIR/isolinux/$initrd"
    else
	# Uubntu does not.
	kernel="$LOOPDIR/$kernel"
	initrd="$LOOPDIR/$initrd"
    fi

    [[ $kernel && -f $kernel && $kernel_params && $initrd && -f $initrd ]] || \
	die -1 "Could not find our kernel!"
	# create our admin disk image
    update_status admin "Creating disk image"
    screen -S "$SCREENNAME" -X screen -t Status "$HOME/test_framework/watch_Status.sh"
    qemu-img create -f raw "$testdir/admin.disk" 16G &>/dev/null

    # makenics populates vm_nics with the appropriate information for
    # run_kvm.  This part cannot run in a subshell, because it relies
    # on getmac being able to hand out unique mac addresses.
    kernel_params+=" crowbar.url=http://192.168.124.10:8091/config crowbar.debug.logdest=/dev/ttyS0 crowbar.use_serial_console=true"
    [[ $DISPLAY ]] || kernel_params+=" console=ttyS1,115200n81"
    [[ -r $HOME/.ssh/id_rsa.pub ]] && kernel_params+=" crowbar.authkey=$(sed 's/ /\\040/g' <"$HOME/.ssh/id_rsa.pub")"
    if ! [[ $manual_deploy = true ]]; then
	kernel_params+=" crowbar.hostname=admin.pod.cloud.openstack.org"
    fi
    if [[ $develop_mode ]]; then
	kernel_params+=" crowbar.debug"
    fi
    update_status admin "Performing install from ${ISO##*/}"
    # First run of the admin node.  Note that we do not actaully boot off the
    # .iso image, instead we boot the vm directly using the extracted kernel
    # and initrd, and arrange for the kernel arguments to contain the 
    # extra arguments that the test framework needs.
    if ! run_kvm -timeout 1200 "$nodename" \
	-cdrom "$ISO" -kernel "$kernel" -initrd "$initrd" \
	-append "$kernel_params"; then
	update_status "$nodename" "Failed to install admin node after 1200 seconds."
	update_status "$nodename" "Node failed to deploy."
	return 1
    fi
    
    # Once this is finished, we no longer need the .iso image mounted.
    sudo /bin/umount -d "$LOOPDIR" &>/dev/null
    
    # restart the admin node as a daemon, and wait for it to be ready to
    # start installing compute nodes.
    update_status admin "Deploying admin node crowbar tasks"
    if ! run_kvm -reboot -timeout 1800 -bootc \
	-dieif "$FRAMEWORKDIR/test_admin_deploy.sh" \
	-daemonif "$FRAMEWORKDIR/check_ready admin.pod.cloud.openstack.org" \
	"$nodename"; then
	update_status admin "Node failed to deploy."
	return 1
    fi
    # Once the KVM instance has launched, start watching the system log.
    screen -S "$SCREENNAME" -X screen -t "Syslog Capture" \
	tail -f "$LOGDIR/admin.2/ttyS0.log"
    # Grab the latest crowbar CLI code off the admin node.
    (   cd "$HOME/testing/cli"
	curlargs=(-L -o - --connect-timeout 120 --max-time 120)
	[[ $CROWBAR_KEY ]] && curlargs+=(-u "$CROWBAR_KEY" --digest) || :
	curl "${curlargs[@]}" "http://${CROWBAR_IP}:3000/support/get_cli" | \
	    tar xzvf -
	# make run_on
	cat >"$HOME/testing/cli/run_on" <<"EOF"
#!/bin/bash
target="$1"
shift
[[ $1 ]] || exit 1
ip_re='([0-9]{1,3}\.){3}[0-9]{1,3}'
addr_re='Address: ([0-9.]+)'
if ! [[ $target =~ $ip_re ]]; then
    if [[ ! $(nslookup "$target" "$CROWBAR_IP") =~ $addr_re ]]; then
       echo "Could not find IP address $target" >&2
       exit 1
    fi
    target=${BASH_REMATCH[1]}
fi
ssh -q -o "UserKnownHostsFile /dev/null" -o "StrictHostKeyChecking no" \
    -o "PasswordAuthentication no" root@"$target" "$@"
EOF
	# make a cheesy knife command
	cat >"$HOME/testing/cli/knife" <<"EOF"
#!/bin/bash
run_on "$CROWBAR_IP" knife "$@"
EOF
	chmod 755 "$HOME/testing/cli"/*
    ) || :
    run_on "$CROWBAR_IP" cp /root/.ssh/authorized_keys \
	/tftpboot/ubuntu_dvd/authorized_keys || :
    # if there are any tests we should run on the admin node, do them now.
    if run_hooks admin_deployed; then
	update_status admin "Admin deploy tests passed"
    else
	update_status admin "Admin deploy tests failed."
	local ret=1
    fi
    
    # If we were asked to pause, do so.
    if [[ $pause_after_admin ]]; then
	pause
    fi
    update_status admin "Node deployed."
    get_cluster_logs admin-deployed
    return ${ret:-0}
}

# Create infrastructure for our slave nodes, but do not start them.
create_slaves() {    
    local nodename
    # First, handle virtual machines.
    for nodename in ${VIRT_NODES[@]}; do
	local vm_nics=()
	makenics "$nodename"
	# Clear out our status, if one is lying around.
	>"$testdir/$nodename.status"
	>"$testdir/$nodename.reset"
	qemu-img create -f raw "$testdir/$nodename.disk" 10G &>/dev/null
        # Create a second and third image for Swift testing
	qemu-img create -f qcow2 "$testdir/$nodename-01.disk" 1G &>/dev/null
	qemu-img create -f qcow2 "$testdir/$nodename-02.disk" 1G &>/dev/null
        # Run our VMs in the background, looping as we go.
	(
	    # Keep rebooting as long as the admin node is alive and 
	    # the node has not been explicitly been killed by kill_vm.
	    count=0
	    in_reset=false
	    while [[ -f $testdir/admin.pid && \
		! -f $testdir/$nodename.killed ]]; do
		# If we are in a reset state, spin until the .reset file is gone
		# or we were asked to die.
    
		if [[ -f $testdir/$nodename.reset ]]; then
		    # If we were asked to reset the node, nuke it.
		    # For whatever reason if we just nuke the MBR d/i
		    # complains about the LVM VG even though it should not.
		    if [[ $in_reset != true ]]; then
			update_status "$nodename" "Creating disk image"
			rm -f "$testdir/$nodename"*.disk
			qemu-img create -f raw "$testdir/$nodename.disk" 6G &>/dev/null
	            # Create a second and third image for Swift testing
			qemu-img create -f qcow2 "$testdir/$nodename-01.disk" 1G &>/dev/null
			qemu-img create -f qcow2 "$testdir/$nodename-02.disk" 1G &>/dev/null
			in_reset=true
		    fi

		    sleep 10
		    continue
		fi
		in_reset=false
		
		# If we have a valid MBR on our primary disk, boot to it.
		# This works around a bug in KVM where you cannot boot to a
		# local disk if you are asked to while PXE booting.
		if [[ $(hexdump -n 2 -s 0x1fe "$testdir/$nodename.disk") =~ \
		    aa55 ]]; then
		    update_status "$nodename" "Booting node to disk ($((count++)))"
		    if run_kvm -bootc "$nodename"; then
			kill_vm "$nodename" exited
		    else
			update_status "$nodename" "Node failed to deploy."
			kill_vm "$nodename" exited
		    fi
		else
		    # Otherwise, PXE boot the machine.
		    update_status "$nodename" "PXE booting node ($((count++)))"
		    if run_kvm -bootn -timeout 1200 "$nodename"; then
			kill_vm "$nodename" exited
		    else
			update_status "$nodename" "Node failed to deploy"
			kill_vm "$nodename" timeout
		    fi
		fi
	    done
	    update_status "$nodename" "Asked to be killed, giving up."
	    exit 0
	) 2>"$testdir/$nodename.errlog" &
    done
    # Second, handle our physical machines.
    local mac
    for mac in "${PHYSICAL_MACS[@]}"; do
	nodename="physical-$((i++))"
	SLAVES["$nodename"]="d${mac//:/-}.pod.cloud.openstack.org"
	> "$testdir/$nodename.status"
    done
}

# Deregister a node from Crowbar.
# The next time it boots Crowbar will restart the discovery cycle for it.
# $1 = the hostname of the node to deregister.
deregister_slave() {
    # make Chef forget about the node.
    crowbar machines delete "$1"
}

# Reset nodes from Crowbar's standpoint -- that is,
# remove them from the Chef database and power them off.
# If the node is a VM, place it in the reset state.
reset_slaves() {
    local slave hname
    if [[ $develop_mode = true ]]; then
	pause "Press any key to let slaves shut down."
    fi
    for slave in "${!SLAVES[@]}"; do
	hname=${SLAVES["$slave"]}
	update_status "$slave" "Asking $hname to shut down"
	# If this is a virtual node, put it in reset state.
	if [[ $slave =~ virt ]]; then
	    > "$testdir/$slave.reset"
	fi
	run_on "$hname" poweroff || :
    done
    echo "$(date '+%F %T %z'): Waiting for 2 minutes to allow nodes to power off"
    [[ $develop_mode ]] || sleep 120
    for slave in "${!SLAVES[@]}"; do
	hname=${SLAVES["$slave"]}
	update_status "$slave" "Forcing $hname to shut down"
	if [[ $slave =~ virt ]]; then
	    kill_vm "$slave" reset || :
	else
	    crowbar machines shutdown "$hname"
	fi
	deregister_slave "$hname"
    done
    sleep 30 # Make sure the delete finishes the chef-client run.
}

# Wake up any slaves that are either powered off or in a reset state.
wakeup_slaves() {
    local i=0 slave='' mac=''
    for slave in "${!SLAVES[@]}"; do
	> "$testdir/$slave.status"
	echo "${SLAVES["$slave"]}" >"$testdir/$slave.hostname"
	case $slave in
	    virt*) rm -f "$testdir/$slave.reset";;
	    physical*) mac="${SLAVES["$slave"]%%.*}"
		mac=${mac//-/:}
		wakeonlan -i 192.168.124.255 "${mac#d}"
		sleep 1;;
	esac
    done
}

# Deploy our slave nodes.
deploy_nodes() {
    wakeup_slaves
    # give things time to launch
    sleep 60
    # Arrange to have the $testdir/$node.status file updated appropriatly
    # when we think the node is deployed
    local node
    for node in "${!SLAVES[@]}"; do
	( 
	    hname=${SLAVES["$node"]}
	    lastres='' res=''
	    while [[ -f $testdir/admin.pid && \
		    ! -f $testdir/$node.killed ]]; do
		if res=$("$FRAMEWORKDIR/check_ready" "$hname"); then
		    update_status "$node" "$res"
		    update_status "$node" "Node deployed."
		    exit 0
		elif [[ $res =~ [Dd]iscovered ]]; then
		    update_status "$node" "$res"
		    update_status "$node" "$(crowbar machines allocate "$hname")"
		elif [[ $lastres != $res ]]; then
		    update_status "$node" "$res"
		    lastres="$res"
		fi
		sleep 10
	    done
	    
	) &
    done
    # Wait for up to $COMPUTE_DEPLOY_WAIT seconds for our nodes to finish 
    # deploying.
    local deadline=$(($(date +%s) + $COMPUTE_DEPLOY_WAIT))
    final_status=Waiting
    while (($deadline > $(date +%s))) && \
	[[ $final_status = Waiting && -f $testdir/admin.pid ]]; do
	final_status=Passed
	for status in "$testdir/"*.status; do
	    [[ -f $status ]] || continue
	    if grep -q 'Node failed to deploy' "$status"; then
		final_status=Failed
		return 1
	    elif ! grep -q 'Node deployed' "$status"; then
		final_status=Waiting
	    fi
	done
	sleep 10
    done
    [[ $final_status = Passed ]] || return 1
}

# run hooks.  They will be sorted in lexicographic order, 
# so naming them with numeric prefixes indicating the order 
# they should run in is a Good Idea.
run_hooks() {
    # $1 = hooks to run.  Maps to $FRAMEWORKDIR/$1_hooks/.
    # $2 = time to allow the tests to run for.
    local deadline=$(($(date '+%s') + ${2:-300}))
    (   sleep 1
	for hook in "$FRAMEWORKDIR/$1"/*.hook; do
	    if [[ -x $hook ]]; then 
		if "$hook"; then
		    continue
		else
		    echo "Failed" >"$testdir/$1.test"
		    exit
		fi
	    fi
	done
	echo "Passed" >"$testdir/$1.test"
    ) &
    local testpid=$!
    sudo "$FRAMEWORKDIR/make_cgroups.sh" $testpid "crowbar-test/$1-test"
    (cd /proc/$testpid
	while [[ -f cmdline ]] && (($(date '+%s') <= $deadline)); do
	    sleep 10
	done
	if [[ -f cmdline ]]; then
	    echo "Timed Out" >"$testdir/$1.test"
	fi
    )
    case $(cat "$testdir/$1.test") in
	Passed) return 0;;
	Failed) return 1;;
	*) 
	    for t in $(cat "$CGROUP_DIR/$1-test/tasks"); do
		kill -9 "$t"
	    done
	    return 1;;
    esac
}

pause() { printf "\n%s\n" "${1:-Press any key to continue:}"; read -n 1; } 

# This is the primary test-running function.
run_test() {
    # If something already holds the testing lock, it is not safe to continue.
    flock -n -x 100 || die -1 "Could not grab $TESTING_LOCK in run_test"
    
    # kill any already running screen sessions
    screen_re="([0-9]+\.$SCREENNAME)"
    while read line; do
	[[ $line =~ $screen_re ]] && screen -S "${BASH_REMATCH[1]}" -X quit || :
    done < <(screen -ls)
    local tests_to_run=() test_results=()
    # Process our commandline arguments.
    while [[ $1 ]]; do
	case $1 in
	    pause) pause_after_deploy=true;;
	    pause-after-admin) pause_after_admin=true;;
	    admin-only) admin_only=true;;
	    develop-mode) develop_mode=true;;
	    manual-deploy) manual_deploy=true;;
	    scratch) tests_to_run+=("$1") ;;
	    use-iso) shift;
		[[ -f $1 ]] || die "Cannot find requested ISO $1"
		ISO="$1";;
	    *) [[ -d $FRAMEWORKDIR/${1}_test ]] || \
		die 1 "Unknown test or option $1"
		tests_to_run+=("$1");;
	esac
	shift
    done

    # Try to grab the latest crowbar iso file to deploy from.
    [[ $ISO ]] || ISO=$(find_latest_iso) || \
	die -1 "Could not populate testing directory, nothing to test."
    # $testdir is where we will store all our disk images and logfiles.
    testdir="$HOME/testing/${ISO##*/}"
    testdir="${testdir%.iso}"
    
    for d in image logs; do mkdir -p "$testdir/$d"; done
    LOOPDIR="$testdir/image"
    export LOGDIR="$testdir/logs"
    # Make sure we clean up after ourselves no matter how we exit.
    trap cleanup 0 INT TERM QUIT
    cd "$HOME/testing"
    # If no test was passed, run the fake test
    [[ $tests_to_run ]] || tests_to_run+=("scratch")
    
    # make a screen session so that we can watch what we are doing if needed.
    screen -d -m -S "$SCREENNAME" -t 'Initial Shell' /bin/bash
    # give a hard status line
    screen -S "$SCREENNAME" -X hardstatus alwayslastline "%?%{yk}%-Lw%?%{wb}%n*%f %t%?(%u)%?%?%{yk}%+Lw%?"
    # enable logging.
    screen -S "$SCREENNAME" -X logfile "$LOGDIR/screenlog.%t"
    # Build our local virtual network.
    make_virt_net
    final_status=Failed
    # Launch our admin node, and then launch the compute nodes afterwards.
    if run_admin_node 2>"$testdir/admin.errlog"; then
	test_results+=("deploy_admin: Passed")
	if [[ $admin_only ]]; then
	    final_status=Passed
	    cleanup
	fi
	create_slaves
	for running_test in "${tests_to_run[@]}"; do
	    if ! deploy_nodes; then
		test_results+=("$running_test: Failed")
		echo "$(date '+%F %T %z'): $running_test deploy failed."
		get_cluster_logs "$running_test-deploy-failed"
		reset_slaves
		continue
	    fi
            echo "$(date '+%F %T %z'): $running_test deploy passed."
	    if ! run_hooks ${running_test}_test 900; then
		echo "$(date '+%F %T %z'): $running_test tests failed."
		test_results+=("$running_test: Failed")
		get_cluster_logs "$running_test-tests-failed"
		reset_slaves
		continue
	    fi
	    echo "$(date '+%F %T %z'): $running_test tests passed."
	    get_cluster_logs "$running_test-tests-passed"
	    test_results+=("$running_test: Passed")
	    if [[ $pause_after_deploy || -f $testdir/pause ]]; then
		pause
	    fi
	    reset_slaves
	done
    else
	test_results=("Admin node: Failed")
    fi
    [[ "${test_results[*]}" =~ Failed ]] || final_status=Passed
    cleanup
} 100>"$TESTING_LOCK"

do_help() {
    cat <<EOF
$0: Crowbar test framework.
I smoketest Crowbar to ensure we have certian minimal functionality.

If you run me without options, I will run a smoketest 
If you run me with run-failed <machine list>, I will try to run
vms from the last failed run.

Read my comments to learn how to tweak me.
EOF
}

[[ -f $HOME/.ssh/known_hosts ]] && rm -f "$HOME/.ssh/known_hosts" || :
[[ -f $HOME/.ssh/id_rsa.pub ]] || { mkdir -p "$HOME/.ssh"; ssh-keygen -q -f "$HOME/.ssh/id_rsa" -t rsa -N '' ; }

[[ -f $HOME/testing/cli ]] || mkdir -p "$HOME/testing/cli"
export PATH="$HOME/testing/cli:$PATH"

export SMOKETEST_PID=$$
CGROUP_DIR=$(sudo "$FRAMEWORKDIR/make_cgroups.sh" \
    $SMOKETEST_PID crowbar-test) || \
    die 1 "Could not mount cgroup filesystem!"


case $1 in
    run-test) shift; run_test "$@" ;;
    build-and-test) shift;
	which build_crowbar.sh &>/dev/null || \
	    die 1 "Cannot find build_crowbar.sh -- have you added the crowbar checkout to \$PATH?"
	echo "$(date '+%F %T %z'): Building Crowbar from source."
	build_crowbar.sh
	echo "$(date '+%F %T %z'): Finshed building Crowbar."
	run_test "$@";;
    cleanup) shift; 
	for l in "$TESTING_LOCK" "$KVM_LOCK" "$CLEANUP_LOCK"; do
	    rm -f "$l"
	done
	cleanup;;
    '') run_test nova swift "$@";;
    *) do_help;;
esac
