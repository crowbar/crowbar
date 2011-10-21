#!/bin/bash
#
# Script: instal-chef.sh
#
# Copyright (c) 2011 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

export FQDN="$1"
export PATH="/opt/dell/bin:$PATH"
export DEBUG=true
[[ ! $HOME || $HOME = / ]] && export HOME="/root"
mkdir -p "$HOME"
die() { echo "$(date '+%F %T %z'): $@"; exit 1; }

crowbar_up=
admin_node_up=

# Run a command and log its output.
log_to() {
    # $1 = install log to log to
    # $@ = rest of args
    local __logname="$1" _ret=0
    local __log="/var/log/install-$1"
    local __timestamp="$(date '+%F %T %z')"
    local log_skip_re='^gem|knife$'
    shift
    printf "\n%s\n" "$__timestamp: Running $*" | \
	tee -a "$__log.err" >> "$__log.log"
    "$@" 2>> "$__log.err" >>"$__log.log" || {
	_ret=$?
	if ! [[ $__logname =~ $log_skip_re ]]; then
	    echo "$__timestamp: $* failed."
	    echo "See $__log.log and $__log.err for more information."
	fi
    }
    printf "\n$s\n--------\n"  "$(date '+%F %T %z'): Done $*" | \
	tee -a "$__log.err" >> "$__log.log"
    return $_ret
}

chef_or_die() {
    if [ -e /opt/dell/bin/blocking_chef_client.sh ]; then
        log_to chef blocking_chef_client.sh && return
    else
        log_to chef chef-client && return
    fi
    if [[ $crowbar_up && $FQDN ]]; then
	crowbar crowbar transition "$FQDN" problem
    fi
    # If we were left without an IP address, rectify that.
    ip link set eth0 up
    ip addr add 192.168.124.10/24 dev eth0
    die "$@"
}

# Run knife in a loop until it doesn't segfault.
knifeloop() {
    local RC=0
    while { log_to knife knife "$@" -u chef-webui -k /etc/chef/webui.pem
	RC=$?
	(($RC == 139)); }; do
	:
    done
}

# Keep trying to start a service in a loop.
# $1 = service to restart
# $2 = status messae to print.
restart_svc_loop() {
    while service "$1" status | egrep -qi "fail|stopped"
    do
        echo "$(date '+%F %T %z'): $2..."
	log_to svc service "$1" start
	sleep 1
    done
}

# Include OS specific functionality
. chef_install_lib.sh || die "Could not include OS specific functionality"

# Verify that our install bits are intact.
if [[ ! -f $DVD_PATH/sha1_passed ]]; then
    (cd $DVD_PATH && sha1sum -c sha1sums &>/dev/null) || \
    die "SHA1sums do not match, install is corrupt."
    >$DVD_PATH/sha1_passed
fi

# If we don't have a key for root, make one and make sure it will get
# copied out correctly.
[[ -f $HOME/.ssh/id_rsa ]] || {
    mkdir -p "$HOME/.ssh"
    ssh-keygen -q -b 2048 -P '' -f "$HOME/.ssh/id_rsa"
    cat "$HOME/.ssh/id_rsa.pub" >> "$HOME/.ssh/authorized_keys"
    cp "$HOME/.ssh/authorized_keys" "$DVD_PATH/authorized_keys"
    cat "$HOME/.ssh/id_rsa.pub" >> /opt/dell/barclamps/provisioner/chef/cookbooks/provisioner/templates/default/authorized_keys.erb

}

fqdn_re='^[0-9a-zA-Z.-]+$'
# Make sure there is something of a domain name
DOMAINNAME=${FQDN#*.}
[[ $DOMAINNAME = $FQDN || $DOMAINNAME = ${DOMAINNAME#*.} ]] && \
    die "Please specify an FQDN for the admin name"
[[ $FQDN =~ $fqdn_re ]] || \
    die "Please specify an FQDN for the admin name with valid characters"

echo "$(date '+%F %T %z'): Setting Hostname..."
update_hostname || die "Could not update our hostname"

# Set up our eth0 IP address way in advance.
# Deploying Crowbar should also do this for us, but sometimes it does not.
# When it does not, things get hard to debug pretty quick.
(ip link set eth0 up; ip addr add 192.168.124.10/24 dev eth0 ) &>/dev/null || :

# once our hostname is correct, bounce rsyslog to let it know.
log_to svc service rsyslog restart || :

echo "$(date '+%F %T %z'): Installing Basic Packages"
install_base_packages || die "Base OS package installation failed."

# This is ugly, but there does not seem to be a better way
# to tell Chef to just look in a specific location for its gems.
echo "$(date '+%F %T %z'): Arranging for gems to be installed"
(   cd $DVD_PATH/extra/gems
    gem install --local --no-ri --no-rdoc builder*.gem
    gem install --local --no-ri --no-rdoc json*.gem
    gem install --local --no-ri --no-rdoc net-http-digest_auth*.gem
    gem install --local --no-ri --no-rdoc activesupport*.gem
    cd ..
    gem generate_index)
# Of course we are rubygems.org. Anything less would be uncivilised.
sed -i -e 's/\(127\.0\.0\.1.*\)/\1 rubygems.org/' /etc/hosts

echo "$(date '+%F %T %z'): Installing Chef"
bring_up_chef || die "Could not start Chef!"

restart_svc_loop chef-solr "Restarting chef-solr - spot one"

chef_or_die "Initial chef run failed"

echo "$(date '+%F %T %z'): Building Keys..."

# Hack up sshd_config to kill delays
sed -i -e 's/^\(GSSAPI\)/#\1/' \
    -e 's/#\(UseDNS.*\)yes/\1no/' /etc/ssh/sshd_config
restart_ssh || :

sed -i "s/pod.your.cloud.org/$DOMAINNAME/g" \
    /opt/dell/barclamps/dns/chef/data_bags/crowbar/bc-template-dns.json

# generate the machine install username and password
CROWBAR_FILE="/opt/dell/barclamps/crowbar/chef/data_bags/crowbar/bc-template-crowbar.json"
if [[ -e $DVD_PATH/extra/config/crowbar.json ]]; then
  CROWBAR_FILE="$DVD_PATH/extra/config/crowbar.json"
fi
mkdir -p /opt/dell/crowbar_framework
CROWBAR_REALM=$(parse_node_data $CROWBAR_FILE -a attributes.crowbar.realm)
CROWBAR_REALM=${CROWBAR_REALM##*=}
if [[ ! -e /etc/crowbar.install.key && $CROWBAR_REALM ]]; then
    dd if=/dev/urandom bs=65536 count=1 2>/dev/null |sha512sum - 2>/dev/null | \
	(read key rest; echo "machine-install:$key" >/etc/crowbar.install.key)
fi

if [[ $CROWBAR_REALM && -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
    sed -i -e "s/machine_password/${CROWBAR_KEY##*:}/g" $CROWBAR_FILE
fi

# Crowbar will hack up the pxeboot files appropriatly.
# Set Version in Crowbar UI
VERSION=$(cat $DVD_PATH/dell/Version)
sed -i "s/CROWBAR_VERSION = .*/CROWBAR_VERSION = \"${VERSION:=Dev}\"/" \
    /opt/dell/barclamps/crowbar/crowbar_framework/config/environments/production.rb

# Right now, we will only try to deploy the OS that the admin node has
# installed.  Eventaully this will go away and be replaced by something
# that is a little mode flexible.
fix_up_os_deployer || die "Unable to fix up OS deployer"

# Installing Barclamps (uses same library as rake commands, but before rake is ready)

# Always run crowbar barclamp first
echo "$(date '+%F %T %z'): Installing Crowbar barclamp..."
log_to bcinstall /opt/dell/bin/barclamp_install.rb \
    "/opt/dell/barclamps/crowbar" || \
    die "Could not install crowbar barclamp."

# Barclamp preparation (put them in the right places)
cd /opt/dell/barclamps
for i in *; do
    [[ -f $i/crowbar.yml && $i != crowbar ]] || continue
    echo "$(date '+%F %T %z'): Installing $i barclamp..."
    log_to bcinstall /opt/dell/bin/barclamp_install.rb \
	"/opt/dell/barclamps/$i" || \
	die "Could not install $i barclamp."
done

restart_svc_loop chef-solr "Restarting chef-solr - spot two"

echo "$(date '+%F %T %z'): Validating data bags..."
log_to validation validate_bags.rb /opt/dell/chef/data_bags || \
    die "Crowbar configuration has errors.  Please fix and rerun install."

echo "$(date '+%F %T %z'): Update run list..."
for role in crowbar deployer-client; do
    knifeloop node run_list add "$FQDN" role["$role"] || \
	die "Could not add $role to Chef. Crowbar bringup will fail."
done

log_to svc service chef-client stop
restart_svc_loop chef-solr "Restarting chef-solr - spot three"

pre_crowbar_fixups

echo "$(date '+%F %T %z'): Bringing up Crowbar..."
# Run chef-client to bring-up crowbar server
chef_or_die "Failed to bring up Crowbar"
# Make sure looper_chef_client is a NOOP until we are finished deploying
touch /tmp/deploying

# have chef_or_die change our status to problem if we fail
crowbar_up=true

# Add configured crowbar proposal
if [ "$(crowbar crowbar proposal list)" != "default" ] ; then
    proposal_opts=()
    if [[ -e $DVD_PATH/extra/config/crowbar.json ]]; then
        proposal_opts+=(--file $DVD_PATH/extra/config/crowbar.json)
    fi
    proposal_opts+=(proposal create default)

    # Sometimes proposal creation fails if Chef and Crowbar are not quite 
    # fully prepared -- this can happen due to solr not having everything
    # fully indexed yet.  So we don't want to just fail immediatly if 
    # we fail to create a proposal -- instead, we will kick Chef, sleep a bit,
    # and try again up to 5 times before bailing out.
    for ((x=1; x<6; x++)); do
        crowbar crowbar "${proposal_opts[@]}" && { proposal_created=true; break; }
        echo "Proposal create failed, pass $x.  Will kick Chef and try again."
        chef_or_die "Kicking proposal bits"
        sleep 1
    done
    if [[ ! $proposal_created ]]; then
        die "Could not create default proposal"
    fi
fi
crowbar crowbar proposal show default >/var/log/default-proposal.json
crowbar crowbar proposal commit default || \
    die "Could not commit default proposal!"
crowbar crowbar show default >/var/log/default.json
chef_or_die "Chef run after default proposal commit failed!"

# transition though all the states to ready.  Make sure that
# Chef has completly finished with transition before proceeding
# to the next.

for state in "discovering" "discovered" "hardware-installing" \
    "hardware-installed" "installing" "installed" "readying" "ready"
do
    while [[ -f "/tmp/chef-client.lock" ]]; do sleep 1; done
    printf "$state: "
    crowbar crowbar transition "$FQDN" "$state" || \
        die "Transition to $state failed!"
    chef_or_die "Chef run for $state transition failed!"
done

# OK, let looper_chef_client run normally now.
rm /tmp/deploying

# Spit out a warning message if we managed to not get an IP address
IPSTR=$(crowbar network show default | parse_node_data -a attributes.network.networks.admin.ranges.admin.start)
IP=${IPSTR##*=}
ip addr | grep -q $IP || {
    echo "$(date '+%F %T %z'): eth0 not configured, but should have been."
    echo "Things will probably end badly."
    echo "Going ahead and configuring eth0 with 192.168.124.10."
    ip link set eth0 up
    ip addr add 192.168.124.10/24 dev eth0
}

restart_svc_loop chef-client "Restarting chef-client - spot four"

update_admin_node

# transform our friendlier Crowbar default home page.
cd $DVD_PATH/extra
[[ $IP ]] && sed "s@localhost@$IP@g" < index.html.tmpl >/var/www/index.html

echo "Admin node deployed."

# Run tests -- currently the host will run this.
#/opt/dell/bin/barclamp_test.rb -t || \
#    die "Crowbar validation has errors! Please check the logs and correct."
