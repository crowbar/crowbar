#!/bin/bash

# Library for smoketests.
# Includes common testing functionality

# We only need to be sourced once.
[[ $SMOKETEST_LIB_SOURCED = true ]] && exit 0

# Make sure we know where to find our test binaries
[[ -d $CROWBAR_DIR/testing/cli ]] || mkdir -p "$CROWBAR_DIR/testing/cli"
export PATH="$CROWBAR_DIR/testing/cli:$CROWBAR_DIR/test_framework:$CROWBAR_DIR:$CROWBAR_DIR/change-image/dell:$PATH:/sbin:/usr/sbin"
set -o pipefail

SMOKETEST_RESULTS=()
# Commands we have to run under sudo -n
SUDO_CMDS="brctl ip umount mount make_cgroups.sh"
# Commands we need to run as $USER
NEEDED_CMDS="ruby gem kvm screen qemu-img sudo"
# Gems we have to have installed.
NEEDED_GEMS="json net-http-digest_auth"
declare -a SMOKETEST_VLANS
SMOKETEST_VLANS[200]="192.168.125.1/24"
SMOKETEST_VLANS[300]="192.168.126.1/24"
SMOKETEST_VLANS[500]="192.168.127.1/24"
SMOKETEST_VLANS[600]="192.168.128.1/24"

# THis lock is held whenever we are running tests.  It exists to
# prevent multiple instances of the smoketest from running at once.
SMOKETEST_LOCK="$CROWBAR_DIR/testing/.testing.lck"

# This lock is held whenever we are starting or killing KVM.
# We use it to ensure that only one virtual machine is being set
# up or torn down at any given time, and that our teardown
# function does not race with our launching function.
SMOKETEST_KVM_LOCK="$CROWBAR_DIR/testing/.kvm.lck"

# This lock is held whenever we are performing a cleanup.
# We want to ensure that we never try to run two
# cleanups in parallel.
SMOKETEST_CLEANUP_LOCK="$CROWBAR_DIR/testing/.cleanup.lck"

SMOKETEST_DIR="$CROWBAR_DIR/test_framework"

# We run all KVM instances in a screen session.
SMOKETEST_SCREEN="crowbar-smoketest"

# The number of computer nodes we will end up deploying.
# Each node has 2 gigs of memory, 2 cores, and a 6 gig disk
# image, so make sure your test machine can handle the load.
SMOKETEST_VIRT_NODES=(virt-1 virt-2 virt-3 virt-4)

# Number of seconds we wait for compute nodes to go through a full deploy.
COMPUTE_DEPLOY_WAIT=2400

# IP address of the node that ends up as the admin node.
export CROWBAR_IP=192.168.124.10
export CROWBAR_KEY="crowbar:crowbar"

# The names of the bridges we will create.  Bridge names must be
# 15 characters or less due to kernel name constraints.
# The last part of the name will be used in tap interface name generation.
# Please keep it at 4 characters or less.
SMOKETEST_BRIDGES=(crowbar-pub)

NICS_PER_BRIDGE=2

# An array of physical interfaces and the bridges they should be bound to.
# We need to use real physical interfaces becasue Crowbar assumes
# it can create and destroy vlans as needed.
# Each entry in this array is if the form ifname,bridgename
# PHYSICAL_INTERFACES=(eth1,crowbar-pub)
PHYSICAL_INTERFACES=()

# An array of MAC addresses of the primary interfaces of the physical machines.
# We need to have this information beforehand so that we can send
# Wake On LAN packets to them.
PHYSICAL_MACS=()

# An Assoc Array for test results
declare -A test_hook_results

[[ $SMOKETEST_ISO && -f $SMOKETEST_ISO ]] || \
    SMOKETEST_ISO="$ISO_DEST/$BUILT_ISO"

# Source a local config file.  Settings in it will override the
# ones here.

if [[ -f $HOME/.test-crowbar ]]; then
    . "$HOME/.test-crowbar"
fi

# A hash containing slave name -> hostname mappings.
declare -A SMOKETEST_SLAVES

grep -q cgroup /proc/filesystems || die "We must have cgroups to track our processes for proper cleanup!"

smoketest_update_status() {
    # $1 = VM status to update.
    # $2 = Status update
    local current_date=$(date '+%F %T %z')
    echo "$current_date: $1 - $2"
    {
        flock 66
        echo "$current_date: $2" >> "$smoketest_dir/$1.status"
    } 66>"$smoketest_dir/.$1.status.lck"
}

# make the bridges we will use for testing.
smoketest_make_bridges() {
    local pub_re='pub$'
    local bridge
    for bridge in "${SMOKETEST_BRIDGES[@]}"; do
        sudo -n brctl show |grep -q "$bridge" || \
            sudo -n brctl addbr "$bridge" || \
            die "Could not create $bridge bridge!"
        # Emulate a switch with STP but no portfast.
        sudo -n brctl stp "$bridge" on || \
            die "Could not enable spanning tree protocol on $bridge!"
        sudo -n brctl setfd "$bridge" 2 || \
            die "Could not set forwarding time on $bridge!"
        sudo -n brctl sethello "$bridge" 1 || \
            die "Could not set hello time for $bridge!"
        sudo -n ip link set "$bridge" up || \
            die "Could not set link on $bridge up!"
        if [[ $bridge =~ $pub_re ]]; then
            sudo -n ip addr add 192.168.124.1/24 dev "$bridge"
            for vlan in "${!SMOKETEST_VLANS[@]}"; do
                if [[ $network_mode = *novlan ]]; then
                    sudo -n ip addr add "${SMOKETEST_VLANS[$vlan]}" \
                        dev $bridge
                else
                    sudo -n ip link add link "$bridge" \
                        name "$bridge.$vlan" type vlan id $vlan
                    sudo -n ip link set "$bridge.$vlan" up
                    sudo -n ip addr add "${SMOKETEST_VLANS[$vlan]}" \
                        dev "$bridge.$vlan"
                fi
            done
        fi
    done
    # Bind the physical nics we want to use to the appropriate bridges.
    for iface in "${PHYSICAL_INTERFACES[@]}"; do
        local ifname=${iface%%,*}
        local bridge=${iface##*,}
        sudo -n ip link set "$ifname" up
        sudo -n brctl addif "$bridge" "$ifname"
    done
}

# remove our bridges after we are done with them.
smoketest_kill_bridges() {
    # Ignore any simple errors in this function.
    set +e
    # Unbind any physical nics we have bound.
    for iface in "${PHYSICAL_INTERFACES[@]}"; do
        local ifname=${iface%%,*}
        local bridge=${iface##*,}
        sudo -n ip link set "$ifname" down
        sudo -n brctl delif "$bridge" "$ifname"
    done
    # Tear down the bridges we created.
    for bridge in "${SMOKETEST_BRIDGES[@]}"; do
        if [[ $bridge =~ $pub_re ]]; then
            sudo -n ip addr del 192.168.124.1/24 dev "$bridge"
            for vlan in "${!SMOKETEST_VLANS[@]}"; do
                if [[ $network_mode = *-novlan ]]; then
                    sudo -n ip addr del "${SMOKETEST_VLAN[$vlan]}" \
                        dev "$bridge"
                else
                    sudo -n ip addr del "${SMOKETEST_VLAN[$vlan]}" \
                        dev "$bridge.$vlan"
                    sudo -n ip link set "$bridge.$vlan" down
                    sudo -n ip link del dev "$bridge.$vlan" type vlan id "$vlan"
                fi
            done
        fi
        sudo -n ip link set "$bridge" down
        sudo -n brctl delbr "$bridge"
    done
}

smoketest_get_cluster_logs() (
    set +e
    echo "$(date '+%F %T %z'): Gathering $1 logs, please wait."
    cd "$LOGDIR"
    [[ -f $smoketest_dir/admin.pid ]] || return 1
    local curlargs=(-L -o "$1-$(date '+%Y%m%d-%H%M%S').tar" \
        --connect-timeout 120 --max-time 120)
    [[ $CROWBAR_KEY ]] && curlargs+=(-u "$CROWBAR_KEY" --digest) || :
    http_proxy='' curl "${curlargs[@]}" "http://$CROWBAR_IP:3000/support/logs"
    echo "$(date '+%F %T %z'): Done gathering $1 logs."
)

# Tidy up after ourselves when we exit.
# This take care to make sure we don't have any stray
# child processes left behind (including VMs), tear down any
# network infrastructure we created, and create any success
# or failure logs we need to create.
declare -a smoketest_cleanup_cmds taps

smoketest_cleanup() {
    # We ignore errors in this function.
    set +e
    flock 75
    [[ -d $smoketest_dir ]] || return 0

    killall check_ready
    [[ $develop_mode = true ]] && pause
    # Gather final logs if our admin node is up.
    smoketest_get_cluster_logs final
    # Make sure our virtual machines have been torn down.
    for pidfile in "$smoketest_dir/"*.pid; do
        local vmname=${pidfile##*/}
        vmname=${vmname%.pid}
        [[ $vmname = '*' ]] && continue
        kill_vm "$vmname" || :
    done
    # If there are any commands to run at smoketest_cleanup, run them now.
    for c in "${smoketest_cleanup_cmds[@]}"; do
        eval $c || :
    done
    # If our .iso is still mounted, umount it.
    sudo -n /bin/umount -d "$LOOPDIR" &>/dev/null
    # Tear down out network.
    kill_virt_net &>/dev/null
    # Kill our screen session
    screen -S "$SMOKETEST_SCREEN" -X quit &>/dev/null
    # Kill anything else we make have left behind.
    for task in $(cat "$CGROUP_DIR/tasks"); do
        [[ $task = $$ || $task = $CROWBAR_BUILD_PID ]] && continue
        kill -9 $task
    done
    # Make sure we exit with the right status code.
    # Copy passed and failed logs to the right location.
    [[ $SMOKETEST_RESULTS ]] && {
        for result in "${SMOKETEST_RESULTS[@]}"; do
            echo "$result"
            [[ $result = *Passed ]] && continue
            final_status=Failed
        done
    }
    [[ $final_status ]] || final_status=Passed
    echo "Deploy $final_status."
    target="${smoketest_dir##*/}-$(date '+%Y%m%d-%H%M%S')-${final_status}"
    rm -f "$smoketest_dir/"*.disk || :
    cp "$CROWBAR_DIR/smoketest.log" "$smoketest_dir"
    (cd "$smoketest_dir/.."; \
        tar czf "$currdir/$target.tar.gz" "${smoketest_dir##*/}")
    echo "Logs are available at $currdir/$target.tar.gz."
    rm -rf "$smoketest_dir"
    [[ $final_status = Passed ]]
} 75>"$CROWBAR_DIR/.smoketest_cleanup.lock"

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
    sudo -n ip tuntap add dev "$1" mode tap || \
        die "Could not create tap device $1"
    sudo -n ip link set "$1" up || \
        die "Could not bring link on tap device $1 up!"
    sudo -n brctl addif "$2" "$1" || \
        die "Could not add tap $1 to bridge $2!"
}

# Remove a tap interface we created.
killtap() {
    set +e
    local res_re='(does not exist|Cannot find device)'
    # $1 = device to kill
    # $2 = bridge to detach it from
    while ! [[ $(sudo -n ip link show "$1" 2>&1) =~ $res_re ]]; do
        sudo -n brctl delif "$2" "$1"
        sudo -n ip link set "$1" down
        sudo -n ip tuntap del dev "$1" mode tap
    done
}

# Build up our local virtual net infrastructure.
make_virt_net() {
    smoketest_make_bridges
    local node bridge idx
    for node in admin ${SMOKETEST_VIRT_NODES[@]}; do
        for bridge in "${SMOKETEST_BRIDGES[@]}"; do
            for ((idx=0; idx < NICS_PER_BRIDGE; idx++)); do
                local nic_name="${node}-${idx}-${bridge##*-}"
                maketap "$nic_name" "$bridge"
            done
        done
    done
}

# Tear down our local virtual net infrastructure.
kill_virt_net() {
    set +e
    local node bridge idx
    for node in admin ${SMOKETEST_VIRT_NODES[@]}; do
        for bridge in ${SMOKETEST_BRIDGES[@]}; do
            for ((idx=0; idx < NICS_PER_BRIDGE; idx++)); do
                local nic_name="${node}-${idx}-${bridge##*-}"
                killtap "$nic_name" "$bridge"
            done
        done
    done
    smoketest_kill_bridges
}

# Make MAC addresses for the network interfaces for a VM.
makenics() {
    # $1 = base name for each nic. Must be 9 characters or less.
    vm_nics=()
    local node bridge idx
    for bridge in ${SMOKETEST_BRIDGES[@]}; do
        for ((idx=0; idx < NICS_PER_BRIDGE; idx++));  do
            local nic_name="$1-${idx}-${bridge##*-}"
            getmac
            if [[ $nic_name =~ virt-.-0-pub ]]; then
                SMOKETEST_SLAVES["$1"]="d${MACADDR//:/-}.smoke.test"
            fi
            vm_nics+=("$MACADDR,$nic_name")
        done
    done
}

# Kill a running VM.
kill_vm() (
    flock 65
    # $1 = vmname
    # $2 = state to assign, defaults to killed.
    set +e
    local killsig=TERM killtries=0 killstate="${2:-killed}"
    # If there is no PID file, assume that the VM is already dead.
    [[ -f $smoketest_dir/$1.pid ]] || {
        > "$smoketest_dir/$1.$killstate"
        flock -u 65
        return 0
    }
    local pid=$(cat "$smoketest_dir/$1.pid")
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
        rm -f "$smoketest_dir/$1.pid" &>/dev/null
        > "$smoketest_dir/$1.$killstate"
        flock -u 65
        return 0
    }
    # If the cmdline entry does not exist in /proc/$pid, the process is
    # already dead but the kernel has not finished cleaning up.
    [[ -f cmdline ]] || {
        rm -f "$smoketest_dir/$1.pid" &>/dev/null
        > "$smoketest_dir/$1.$killstate"
        flock -u 65
        return 0
    }
    # If the /proc/$pid/cmdline does not contain the name of our VM,
    # the kernel has finished cleaning up our process and enough processes
    # have been spawned that our PID has been reused.
    grep -q "$1" cmdline || {
        rm -f "$smoketest_dir/$1.pid" &>/dev/null
        > "$smoketest_dir/$1.$killstate"
        flock -u 65
        return 0
    }
    # Loop trying to kill this VM.  Escalate our violence and sleep longer
    # the more tries we take.
    while (( killtries++ < 10)); do
        smoketest_update_status $vmname "Killing $vm_gen (try $killtries, signal $killsig)"
        kill "-$killsig" "$pid"
        ((killtries < 5)) || killsig=KILL
        sleep $killtries
        # if /proc/$pid/cmdline (or any other file that normally exists there)
        # is gone, the process is dead, and our work is done.
        if [[ ! -f cmdline ]]; then
            smoketest_update_status $vmname "Killed with SIG${killsig}"
            rm -f "$smoketest_dir/$vmname.pid" &>/dev/null
            > "$smoketest_dir/$vmname.$killstate"
            flock -u 65
            return 0
        fi
    done
    flock -u 65
    echo"Could not kill $vmname, something went horribly wrong."
    return 1
) 65>"$SMOKETEST_KVM_LOCK"

# Wait for a KVM instance to die naturally, for a timeout to expire,
# or for a daemonization condition to be reached.
wait_for_kvm() {
    # $1 = name of KVM instance.
    local vmname=$1
    shift
    local pidfile="$smoketest_dir/$vmname.pid"
    [[ -f $pidfile ]] || {
        smoektest_update_status "$vmanme" "No pid file for KVM."
        return 1 # no pidfile? Bad Things happened.
    }
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
        [[ -f cmdline && $(cat cmdline) =~ $vmname ]] || {
            smoketest_status_update "$vmanme" "/proc/$kvmpid is not for our VM."
            return 1
        }
        while [[ -f cmdline ]]; do
            # If there is a condition on which we should kill the VM
            # immediatly, test and see if it is true.
            if [[ $dieif ]] && $dieif; then
                smoketest_update_status "$vmname" "Ran into instant-kill condition."
                return 1
            fi
            # If there is a condition on which we should stop waiting for
            # a VM, test to see if it is true.
            if [[ $daemonif ]]; then
                # We assign the output of $daemonif to a variable so that
                # we don't spam up the test run transcript.
                if thisres=$($daemonif 2>&1); then
                    # If it is, stop watching this VM.
                    smoketest_update_status "$vmname" "$thisres"
                    smoketest_update_status "$vmname" \
                        "Daemonizing node with $(($deadline - $(date +%s))) seconds left."
                    return 0
                elif [[ $thisres && $lastres != $thisres ]]; then
                    smoketest_update_status "$vmname" "$thisres"
                    lastres="$thisres"
                fi
            fi
            # If we were supposed to test for a deadline and we overran it,
            # return with the appropriate status code.
            if [[ $deadline && ! $develop_mode ]] && (($(date +%s) > $deadline)); then
                smoketest_update_status "$vmname" "Node ran for more than $timeout seconds."
                return 1
            fi
            sleep 10
        done
        # If we wanted to be daemonized but were not, game over man.
        if [[ $daemonif ]]; then
            smoketest_update_status "$vmname" "Node failed to daemonize."
            return 1
        else
            # We appear to have exited normally.
            smoketest_update_status "$vmname" "Node exited."
            return 0
        fi
    )
}

# Hash that allows us to track the number of reboots a VM has had.
declare -A kvm_generations

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
    # Hack to pick the fastest disk caching mode.
    # We use unsafe caching if we can on the vms because we will just
    # rebuild the filesystems from scratch if anything goes wrong.
    if ! [[ $drive_cache ]]; then
        if kvm --help |grep -q 'cache.*unsafe'; then
            drive_cache=unsafe
        else
            drive_cache=writeback
        fi
        if kvm -device \? 2>&1 |grep -q ahci && \
            [[ $(kvm -version) =~ kvm-1 ]]; then
            kvm_use_ahci=true
        fi
    fi
    local vm_gen="$vmname.${kvm_generations[$vmname]}"
    # create a new log directory for us.  vm_logdir needs to be global
    # because other things will need to read the logs to see what is happening.
    vm_logdir="$smoketest_dir/logs/$vm_gen"
    mkdir -p "$vm_logdir"
    local pidfile="$smoketest_dir/$vmname.pid"
    [[ -f $pidfile ]] && rm "$pidfile" || :
    # Our initial array of args for kvm.
    # We pass in our own ROM for PXE booting to work around some old
    # PXE boot bugs in older versions of the E1000 etherboot roms.
    local kvmargs=(-enable-kvm
        -m $mem_size
        -smp $cpu_count
        -pidfile "$pidfile"
        -serial "file:$vm_logdir/ttyS0.log"
        -serial "file:$vm_logdir/ttyS1.log"
        -name "kvm-$vm_gen")
    if [[ $kvm_use_ahci = true ]]; then
        kvmargs+=(-device "ahci,id=ahci0,bus=pci.0,multifunction=on")
        kvmargs+=(-drive "file=$smoketest_dir/$vmname.disk,if=none,format=raw,cache=$drive_cache,id=drive-ahci-0")
        kvmargs+=(-device "ide-drive,bus=ahci0.0,drive=drive-ahci-0,id=drive-0")
        local drive_idx=1
        for image in "$smoketest_dir/$vmname-"*".disk"; do
            [[ -f $image ]] || continue
            kvmargs+=(-device "ahci,id=ahci${drive_idx},bus=pci.0,multifunction=on")
            kvmargs+=(-drive "file=$image,if=none,cache=$drive_cache,id=drive-ahci-${drive_idx}")
            kvmargs+=(-device "ide-drive,bus=ahci${drive_idx}.0,drive=drive-ahci-${drive_idx},id=drive-${drive_idx}")
            drive_idx=$((drive_idx + 1))
        done
        unset drive_idx
    else
        local drivestr="file=$smoketest_dir/$vmname.disk,if=scsi,format=raw,cache=$drive_cache"
        if [[ $driveboot ]]; then
            drivestr+=",boot=on"
        fi
        kvmargs+=(-drive "$drivestr")
        # Add additional disks if we have any.
        for image in "$smoketest_dir/$vmname-"*".disk"; do
            [[ -f $image ]] || continue
            kvmargs+=(-drive "file=$image,if=scsi,format=qcow2,cache=$drive_cache")
        done
    fi
    # Add appropriate nics based on the contents of vm_nics.
    local vlan=0
    for line in "${vm_nics[@]}"; do
        kvmargs+=(-net "nic,macaddr=${line%%,*},model=e1000,vlan=$vlan")
        kvmargs+=(-net "tap,ifname=${line##*,},script=no,downscript=no,vlan=$vlan")
        vlan=$(($vlan + 1))
    done
    unset vlan
    if [[ $pxeboot ]]; then
        kvmargs+=(-boot "order=n")
    elif [[ $driveboot ]]; then
        kvmargs+=(-boot "order=nc")
    fi

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
            screen -S "$SMOKETEST_SCREEN" -X screen \
                -t "$vm_gen" kvm "${kvmargs[@]}" "$@"
            screen -S "$SMOKETEST_SCREEN" -p "$vm_gen" -X log on
        fi
        # wait up to 10 seconds for a PID file
        local s
        for ((s=0; s<10; s++)); do
            [[ -f $pidfile ]] && break
            sleep 1
        done
        flock -u 65
    } 65>"$SMOKETEST_KVM_LOCK"
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

    smoketest_update_status admin "Creating disk image"
    screen -S "$SMOKETEST_SCREEN" -X screen -t Status "$CROWBAR_DIR/test_framework/watch_Status.sh"
    qemu-img create -f raw "$smoketest_dir/admin.disk" 20G &>/dev/null

    if [[ $online = true ]]; then
        if ! run_kvm "$nodename" -cdrom "$SMOKETEST_ISO" ; then
            smoketest_update_status "$nodename" "Failed to install admin node after 1200 seconds."
            smoketest_update_status "$nodename" "Node failed to deploy."
            return 1
        fi
    else
        smoketest_update_status admin "Mounting .iso"
        sudo -n /bin/mount -o loop "$SMOKETEST_ISO" "$LOOPDIR" &>/dev/null || \
            die "Could not loopback mount $SMOKETEST_ISO on $LOOPDIR."
        smoketest_cleanup_cmds+=("sudo -n /bin/umount -d '$LOOPDIR'")
        
        smoketest_update_status admin "Hacking up kernel parameters"
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
        for d in "$LOOPDIR/isolinux" "$LOOPDIR"; do
            [[ -f $d/$kernel && -f $d/$initrd ]] || continue
            kernel="$d/$kernel"
            initrd="$d/$initrd"
            break
        done
        [[ $kernel && -f $kernel && $kernel_params && $initrd && -f $initrd ]] || \
            die "Could not find our kernel!"
        kernel_params+=" crowbar.url=http://192.168.124.10:8091/config crowbar.debug.logdest=/dev/ttyS0 crowbar.use_serial_console=true"
        [[ $DISPLAY ]] || kernel_params+=" console=ttyS1,115200n81"
        [[ -r $HOME/.ssh/id_rsa.pub ]] && kernel_params+=" crowbar.authkey=$(sed 's/ /\\040/g' <"$HOME/.ssh/id_rsa.pub")"
        if ! [[ $manual_deploy = true ]]; then
            kernel_params+=" crowbar.hostname=admin.smoke.test"
        fi
        if [[ $develop_mode ]]; then
            kernel_params+=" crowbar.debug"
        fi
        smoketest_update_status admin "Performing install from ${SMOKETEST_ISO##*/}"
        # First run of the admin node.  Note that we do not actaully boot off the
        # .iso image, instead we boot the vm directly using the extracted kernel
        # and initrd, and arrange for the kernel arguments to contain the
        # extra arguments that the test framework needs.
        if ! run_kvm -timeout 1200 "$nodename" \
            -cdrom "$SMOKETEST_ISO" -kernel "$kernel" -initrd "$initrd" \
            -append "$kernel_params"; then
            smoketest_update_status "$nodename" "Failed to install admin node after 1200 seconds."
            smoketest_update_status "$nodename" "Node failed to deploy."
            return 1
        fi

        # Once this is finished, we no longer need the .iso image mounted.
        sudo -n /bin/umount -d "$LOOPDIR" &>/dev/null
    fi
    # restart the admin node as a daemon, and wait for it to be ready to
    # start installing compute nodes.
    smoketest_update_status admin "Deploying admin node crowbar tasks"
    if ! run_kvm -reboot -timeout 1800 -bootc \
        -daemonif "ping -q -c 1 -t 5 192.168.124.10" \
        "$nodename"; then
        smoketest_update_status admin "Node failed to deploy."
        return 1
    fi
    if [[ $online = true ]]; then
        local cont=false
        while [[ $cont != "continue" ]]; do
            read -p "Type 'continue' when the admin node is deployed." cont
        done
        return 0
    fi
    # Once the KVM instance has launched, start watching the system log.
    screen -S "$SMOKETEST_SCREEN" -X screen -t "Syslog Capture" \
        tail -f "$LOGDIR/admin.2/ttyS0.log"
    #Spin until we can SSH into the admin node.
    while ! ssh root@192.168.124.10 true; do
        sleep 5
    done
    # COpy over the network.json we want to use.
    scp "$CROWBAR_DIR/test_framework/network-${network_mode}.json" \
        "root@192.168.124.10:/opt/dell/barclamps/network/chef/data_bags/crowbar/bc-template-network.json"
    # Copy over our post-install hooks
    ssh root@192.168.124.10 mkdir -p /opt/dell/.hooks/admin-post-install.d
    scp -r "$CROWBAR_DIR/test_framework/admin-post-hooks/." \
        "root@192.168.124.10:/opt/dell/.hooks/admin-post-install.d/"
    # Kick off the install.
    ssh root@192.168.124.10 /opt/dell/bin/install-crowbar admin.smoke.test
    sleep 5
    # Wait for the screen session to terminate
    printf "Waiting for crowbar to install: "
    while grep -q crowbar-install < <(ssh root@192.168.124.10 screen -ls); do
        sleep 30
        printf "."
    done
    echo
    if ! ssh root@192.168.124.10 \
        test -f /opt/dell/crowbar_framework/.crowbar-installed-ok; then
        smoketest_update_status admin "Install of Crowbar failed."
        return 1
    fi
    # Grab the latest crowbar CLI code off the admin node.
    (
        cd "$CROWBAR_DIR/testing/cli"
        curlargs=(-L -o - --connect-timeout 120 --max-time 120)
        [[ $CROWBAR_KEY ]] && curlargs+=(-u "$CROWBAR_KEY" --digest) || :
        http_proxy='' curl "${curlargs[@]}" \
            "http://${CROWBAR_IP}:3000/support/get_cli" | tar xzvf -
        # make run_on
        cat >"$CROWBAR_DIR/testing/cli/run_on" <<"EOF"
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
        cat >"$CROWBAR_DIR/testing/cli/knife" <<"EOF"
#!/bin/bash
run_on "$CROWBAR_IP" knife "$@"
EOF
        chmod 755 "$CROWBAR_DIR/testing/cli"/*
    ) || :
    # if there are any tests we should run on the admin node, do them now.
    if run_admin_hooks admin_deployed; then
        smoketest_update_status admin "Admin deploy tests passed"
    else
        smoketest_update_status admin "Admin deploy tests failed."
        local ret=1
    fi

    # If we were asked to pause, do so.
    if [[ $pause_after_admin ]]; then
        pause
    fi
    smoketest_update_status admin "Node deployed."
    smoketest_get_cluster_logs admin-deployed
    return ${ret:-0}
}

# Deregister a node from Crowbar.
# The next time it boots Crowbar will restart the discovery cycle for it.
# $1 = the hostname of the node to deregister.
deregister_slave() {
    # make Chef forget about the node.
    crowbar machines delete "$1"
}

# Create infrastructure for our slave nodes, but do not start them.
create_slaves() {
    local nodename
    # First, handle virtual machines.
    for nodename in ${SMOKETEST_VIRT_NODES[@]}; do
        local vm_nics=()
        makenics "$nodename"
        # Clear out our status, if one is lying around.
        >"$smoketest_dir/$nodename.status"
        >"$smoketest_dir/$nodename.reset"
        qemu-img create -f raw "$smoketest_dir/$nodename.disk" 10G &>/dev/null
        # Create a second and third image for Swift testing
        qemu-img create -f qcow2 "$smoketest_dir/$nodename-01.disk" 4G &>/dev/null
        qemu-img create -f qcow2 "$smoketest_dir/$nodename-02.disk" 4G &>/dev/null
        # Run our VMs in the background, looping as we go.
        (
            trap - 0 INT QUIT TERM
            # Keep rebooting as long as the admin node is alive and
            # the node has not been explicitly been killed by kill_vm.
            count=0
            in_reset=false
            while [[ -f $smoketest_dir/admin.pid && \
                ! -f $smoketest_dir/$nodename.killed ]]; do
                # If we are in a reset state, spin until the .reset file is gone
                # or we were asked to die.

                if [[ -f $smoketest_dir/$nodename.reset ]]; then
                    # If we were asked to reset the node, nuke it.
                    # For whatever reason if we just nuke the MBR d/i
                    # complains about the LVM VG even though it should not.

                    if [[ $in_reset != true ]]; then
                        deregister_slave ${SMOKETEST_SLAVES["$nodename"]}
                        sleep 30
                        qemu-img create -f raw \
                            "$smoketest_dir/$nodename.disk" 10G &>/dev/null
                        in_reset=true
                    fi

                    sleep 10
                    continue
                fi
                in_reset=false

                # If we have a valid MBR on our primary disk, boot to it.
                # This works around a bug in KVM where you cannot boot to a
                # local disk if you are asked to while PXE booting.
                if [[ $(hexdump -n 2 -s 0x1fe "$smoketest_dir/$nodename.disk") =~ \
                    aa55 ]]; then
                    smoketest_update_status "$nodename" "Booting node to disk ($((count++)))"
                    if run_kvm -bootc "$nodename"; then
                        kill_vm "$nodename" exited
                    else
                        smoketest_update_status "$nodename" "Node failed to deploy."
                        kill_vm "$nodename" exited
                    fi
                else
                    # Otherwise, PXE boot the machine.
                    smoketest_update_status "$nodename" "PXE booting node ($((count++)))"
                    if run_kvm -bootn -timeout 1200 "$nodename"; then
                        kill_vm "$nodename" exited
                    else
                        smoketest_update_status "$nodename" "Node failed to deploy"
                        kill_vm "$nodename" timeout
                    fi
                fi
            done
            smoketest_update_status "$nodename" "Asked to be killed, giving up."
            exit 0
        ) &
    done
    # Second, handle our physical machines.
    local mac
    for mac in "${PHYSICAL_MACS[@]}"; do
        nodename="physical-$((i++))"
        SMOKETEST_SLAVES["$nodename"]="d${mac//:/-}.smoke.test"
        > "$smoketest_dir/$nodename.status"
    done
}

kill_slave() {
    local hname=${SMOKETEST_SLAVES["$1"]}
    smoketest_update_status "$1" "Killing $hname"
    if [[ $1 =~ virt ]]; then
        kill_vm "$1" "$@"
    else
        crowbar machines shutdown "$1" || :
    fi
}

kill_slaves() {
    local slave
    for slave in "${!SMOKETEST_SLAVES[@]}"; do
        kill_slave "$slave" "$@"
    done
}

reset_slave() {
    if [[ $1 =~ virt ]]; then
        > "$smoketest_dir/$1.reset"
    fi
    kill_slave "$1" reset
}

# Reset nodes from Crowbar's standpoint -- that is,
# remove them from the Chef database and power them off.
# If the node is a VM, place it in the reset state.
reset_slaves() {
    local slave
    if [[ $develop_mode = true ]]; then
        pause "Press any key to reset the slaves."
    fi
    for slave in "${!SMOKETEST_SLAVES[@]}"; do
        reset_slave "$slave"
    done
}

# Wake up any slaves that are either powered off or in a reset state.
wakeup_slaves() {
    local i=0 slave='' mac=''
    for slave in "${!SMOKETEST_SLAVES[@]}"; do
        > "$smoketest_dir/$slave.status"
        echo "${SMOKETEST_SLAVES["$slave"]}" >"$smoketest_dir/$slave.hostname"
        case $slave in
            virt*) rm -f "$smoketest_dir/$slave.reset";;
            physical*) mac="${SMOKETEST_SLAVES["$slave"]%%.*}"
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
    # Arrange to have the $smoketest_dir/$node.status file updated appropriatly
    # when we think the node is deployed
    local node overall_status=Waiting
    for node in "${!SMOKETEST_SLAVES[@]}"; do
        (
            trap - 0 INT QUIT TERM
            export TARGET_STATE=discovered
            hname=${SMOKETEST_SLAVES["$node"]}
            lastres='' res=''
            while [[ -f $smoketest_dir/admin.pid && \
                    ! -f $smoketest_dir/$node.killed ]]; do
                if res=$(check_ready "$hname"); then
                    smoketest_update_status "$node" "$res"
                    smoketest_update_status "$node" "Node deployed."
                    exit 0
                elif [[ $lastres != $res ]]; then
                    smoketest_update_status "$node" "$res"
                    lastres="$res"
                fi
                sleep 10
            done
            exit 0
        ) &
    done
    # Wait for up to $COMPUTE_DEPLOY_WAIT seconds for our nodes to finish
    # deploying.
    local deadline=$(($(date +%s) + $COMPUTE_DEPLOY_WAIT))
    while (($deadline > $(date +%s))) && \
        [[ $overall_status = Waiting && -f $smoketest_dir/admin.pid ]]; do
        overall_status=Passed
        for status in "$smoketest_dir/"*.status; do
            [[ -f $status ]] || continue
            grep -q 'Node failed to deploy' "$status" && return 1
            if ! grep -q 'Node deployed' "$status"; then
                overall_status=Waiting
                continue
            fi
        done
        sleep 10
    done
    [[ $overall_status = Passed ]]
}

# run hooks.  They will be sorted in lexicographic order,
# so naming them with numeric prefixes indicating the order
# they should run in is a Good Idea.
run_hooks() {
    # $1 = name of the test
    # $2 = path to find the hooks in
    # $3 = Timeout for the tests, defaults to 300 seconds.
    # $4 = Extension for the hooks, defaults to 'hook'
    local test_name="$1" test_dir="$2" timeout=${3:-300} ext=${4:-hook}
    local deadline=$(($(date '+%s') + ${timeout})) hook
    if [[ -d $test_dir ]]; then
        echo "Timed Out" > "$smoketest_dir/$test_name.test"
    else
        echo "Passed" > "$smoketest_dir/$test_name.test"
        return 0
    fi
    (   sleep 1
        trap - 0 INT QUIT TERM
        unset http_proxy https_proxy
        for hook in "$test_dir"/*."$ext"; do
            [[ -x $hook ]] || continue
            echo "$(date '+%F %T %z'): Running test hook ${hook##*/}"
            "$hook" && continue
            echo "Failed" >"$smoketest_dir/$test_name.test"
            exit
        done
        echo "Passed" >"$smoketest_dir/$test_name.test"
        exit
    ) &
    local testpid=$!
    sudo -n "$(which make_cgroups.sh)" $testpid "crowbar-test/${test_name}-test"
    (   cd /proc/$testpid
        while [[ -f cmdline ]] && (($(date '+%s') <= $deadline)); do
            sleep 10
        done)
    case $(cat "$smoketest_dir/$test_name.test") in
        Passed) return 0;;
        Failed) return 1;;
        *)  # We timed out.  Kill everything associated with this test.
            for t in $(cat "$CGROUP_DIR/${test_name}-test/tasks"); do
                kill -9 "$t"
            done
            return 1;;
    esac
}


run_admin_hooks() {
    run_hooks admin "$SMOKETEST_DIR/admin_deployed" 300 hook
}

pause() { printf "\n%s\n" "${1:-Press any key to continue:}"; read -n 1; }

mangle_ssh_config() {
    grep -q 'Host 192\.168\.124\.\*' "$HOME/.ssh/config" && return 0
    cat >>"$HOME/.ssh/config" <<EOF

# Added by Crowbar Smoketest Framework
Host 192.168.124.*
     UserKnownHostsFile /dev/null
     StrictHostKeyChecking no
     CheckHostIP no

EOF
}

# This is the primary test-running function.
run_test() {
    # If something already holds the testing lock, it is not safe to continue.
    flock -n -x 100 || die "Could not grab $SMOKETEST_LOCK in run_test"
    for cmd in $NEEDED_CMDS $SUDO_CMDS; do
        which $cmd &>/dev/null && continue
        echo "Missing required command $cmd (or it is not in \$PATH)."
        echo "Please make sure the following commands are installed:"
        echo "$SUDO_CMDS"
        echo "$NEEDED_CMDS"
        exit 1
    done

    for cmd in $SUDO_CMDS; do
        sudo -l -n "$(which $cmd)" &>/dev/null && continue
        echo "$USER is not allowed to run $(which $cmd) using sudo."
        echo "Please make sure that $USER has passwordless sudo rights to run:"
        printf "%s " $(for cmd in $SUDO_CMDS; do which "$cmd"; done)
        echo
        exit 1
    done

    if ! [[ -c /dev/kvm && -w /dev/kvm ]]; then
        echo "Please make sure that /dev/kvm exists, and that"
        echo "$USER has write permissions on it."
        exit 1
    fi

    for gem in $NEEDED_GEMS; do
        gem list |grep -q $gem && continue
        echo "Missing required gem $gem."
        echo "Please make sure the following gems are installed:"
        echo "$NEEDED_GEMS"
        exit 1
    done

    mangle_ssh_config

    CGROUP_DIR=$(sudo -n "$(which make_cgroups.sh)" $$ crowbar-test) || \
    die "Could not mount cgroup filesystem!"

    # kill any already running screen sessions
    screen_re="([0-9]+\.$SMOKETEST_SCREEN)"
    while read line; do
        [[ $line =~ $screen_re ]] && screen -S "${BASH_REMATCH[1]}" -X quit || :
    done < <(screen -ls)
    local tests_to_run=()
    local network_mode=team
    # Process our commandline arguments.
    while [[ $1 ]]; do
        case $1 in
            pause) local pause_after_deploy=true;;
            pause-after-admin) local pause_after_admin=true;;
            admin-only) local admin_only=true;;
            develop-mode) local develop_mode=true;;
            manual-deploy) local manual_deploy=true;;
            use-iso) shift; SMOKETEST_ISO="$1";;
            single*|dual*|team*) local network_mode="$1";;
            bind-nic) shift;
                [[ -d /sys/class/net/$1 ]] || \
                    die "$1 is not a network interface!"
                is_in "$2" "${SMOKETEST_BRIDGES[*]}" || \
                    die "$2 is not a bridge of ours!"
                PHYSICAL_INTERFACES+=("$1,$2")
                shift;;
            use-screen) unset DISPLAY;;
            online) online=true;;
            local_proxy) proxy="$2"; shift;;
            scratch);;
            *)
                if [[ -d $CROWBAR_DIR/barclamps/$1 ]]; then
                    tests_to_run+=("$1")
                else
                    die "Unknown test or option $1"
                fi;;
        esac
        shift
    done
    [[ -f $CROWBAR_DIR/test_framework/network-${network_mode}.json ]] || \
        die "Cannot use network mode $network_mode, no JSON for it."


    [[ $SMOKETEST_ISO = /* ]] || SMOKETEST_ISO="$PWD/$SMOKETEST_ISO"
    [[ -f $SMOKETEST_ISO ]] || die "Cannot find $SMOKETEST_ISO to test!"
    # $smoketest_dir is where we will store all our disk images and logfiles.
    smoketest_dir="$CROWBAR_DIR/testing/${SMOKETEST_ISO##*/}"
    smoketest_dir="${smoketest_dir%.iso}"
    [[ -d $smoketest_dir ]] && (cd "$smoketest_dir"; rm -rf *)
    [[ $tests_to_run ]] || tests_to_run=("crowbar")

    for d in image logs; do mkdir -p "$smoketest_dir/$d"; done
    LOOPDIR="$smoketest_dir/image"
    export LOGDIR="$smoketest_dir/logs"
    # Make sure we clean up after ourselves no matter how we exit.
    cleanup_cmds+=(smoketest_cleanup)

    cd "$CROWBAR_DIR/testing"
    # make a screen session so that we can watch what we are doing if needed.
    screen -wipe &>/dev/null || :
    screen -d -m -S "$SMOKETEST_SCREEN" -t 'Initial Shell' /bin/bash
    # give a hard status line
    screen -S "$SMOKETEST_SCREEN" -X hardstatus alwayslastline "%?%{yk}%-Lw%?%{wb}%n*%f %t%?(%u)%?%?%{yk}%+Lw%?"
    # enable logging.
    screen -S "$SMOKETEST_SCREEN" -X logfile "$LOGDIR/screenlog.%t"
    # Build our local virtual network.
    make_virt_net
    final_status=Failed
    # Launch our admin node, and then launch the compute nodes afterwards.
    if run_admin_node; then
        SMOKETEST_RESULTS+=("Admin Node: Passed")
        if [[ $admin_only ]]; then
            final_status=Passed
        else
            create_slaves
            for running_test in "${tests_to_run[@]}"; do
                if ! deploy_nodes; then
                    SMOKETEST_RESULTS+=("$running_test: Failed")
                    echo "$(date '+%F %T %z'): Compute node deploy failed."
                    smoketest_get_cluster_logs "$running_test-deploy-failed"
                    reset_slaves
                    continue
                fi
                echo "$(date '+%F %T %z'): Compute nodes deployed."
                for this_test in $running_test; do
                    echo "$(date '+%F %T %z'): Running smoketests for $this_test."
                    if ! run_on $CROWBAR_IP /opt/dell/bin/smoketest "$this_test"; then
                        echo "$(date '+%F %T %z'): $this_test tests failed."
                        SMOKETEST_RESULTS+=("$this_test: Failed")
                        smoketest_get_cluster_logs "$running_test-tests-failed"
                        reset_slaves
                        continue 2
                    fi
                done
                echo "$(date '+%F %T %z'): $running_test tests passed."
                smoketest_get_cluster_logs "$running_test-tests-passed"
                SMOKETEST_RESULTS+=("$running_test: Passed")
                if [[ $pause_after_deploy || -f $smoketest_dir/pause ]]; then
                    pause
                fi
                reset_slaves
            done
        fi
    else
        final_status=Failed
        SMOKETEST_RESULTS=("Admin node: Failed")
    fi
    [[ ! $final_status && $"${SMOKETEST_RESULTS[*]}" =~ Failed ]] || \
        final_status=Passed
    [[ $develop_mode ]] && pause
    kill_slaves || :
    sleep 15
    [[ $final_status = Passed ]]
} 100>"$SMOKETEST_LOCK"

SMOKETEST_LIB_SOURCED=true
