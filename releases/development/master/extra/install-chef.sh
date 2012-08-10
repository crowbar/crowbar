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

if [[ -f /opt/dell/crowbar_framework/.crowbar-installed-ok ]]; then
    echo "Crowbar is already installed, refusing to let install run."
    echo "If you really want to do this, "
    echo "remove /opt/dell/crowbar_framework/.crowbar-installed-ok"
    exit 1
fi

export FQDN="$1"
export PATH="/opt/dell/bin:/usr/local/bin:$PATH"
export DEBUG=true
[[ ! $HOME || $HOME = / ]] && export HOME="/root"
mkdir -p "$HOME"
die() {
    if [[ $crowbar_up && $FQDN ]]; then
        crowbar crowbar transition "$FQDN" problem
    fi
    echo "$(date '+%F %T %z'): $@"
    exit 1
}

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
    [[ -e $__log.log ]] || echo "Install run done for version: $VERSION" >> $__log.log
    [[ -e $__log.err ]] || echo "Install run done for version: $VERSION" >> $__log.err
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

# Sometimes the machine role (crowbar-${FQDN//./_}) does not get properly
# attached to the admin node.  We are in deep trouble if that happens.
check_machine_role() {
    local count
    for ((count=0; count <= 5; count++)); do
        grep -q "crowbar-${FQDN//./_}" < <(knife node show "$FQDN" ) && return 0
        sleep 10
    done
    die "Node machine-specific role got lost.  Deploy failed."
}

# Include OS specific functionality
. chef_install_lib.sh || die "Could not include OS specific functionality"

# Set Version for all to use.
VERSION=$(cat $DVD_PATH/dell/Version)
echo "Installing admin with version: $VERSION"

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
    cp "$HOME/.ssh/authorized_keys" "/tftpboot/authorized_keys"
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

# Link the discovery image to an off-DVD location.
mv "${DVD_PATH}/discovery" "/tftpboot"

echo "$(date '+%F %T %z'): Installing Basic Packages"
install_base_packages || die "Base OS package installation failed."

# Lift the gems off the install media for easy file serving.
mkdir -p /tftpboot/gemsite/gems
find "/opt/dell/barclamps" -path '*/gems/*.gem' \
    -exec ln -sf '{}' /tftpboot/gemsite/gems ';'

# Arrange for all our gem binaries to be installed into /usr/local/bin
cat >/etc/gemrc <<EOF
:sources:
- http://127.0.0.1:3001/
gem: --no-ri --no-rdoc --bindir /usr/local/bin
EOF

# This is ugly, but there does not seem to be a better way
# to tell Chef to just look in a specific location for its gems.
echo "$(date '+%F %T %z'): Arranging for gems to be installed"
(   cd /tftpboot/gemsite/gems
    for gem in builder json net-http-digest_auth activesupport i18n \
        daemons bluepill xml-simple libxml-ruby wsman cstruct ; do
        gem install --local --no-ri --no-rdoc $gem-*.gem
    done
    cd ..
    gem generate_index)

mkdir -p /var/run/bluepill
mkdir -p /var/lib/bluepill
mkdir -p /etc/bluepill

# Copy all our pills to
cp "$DVD_PATH/extra/"*.pill /etc/bluepill
cp "$DVD_PATH/extra/chef-server.conf" /etc/nginx
cp "$DVD_PATH/extra/chef-client.pill" /tftpboot

# Fire up a Webrick instance on port 3001 to serve gems.
echo "$(date '+%F %T %z'): Arranging for gems to be served from port 3001"
mkdir -p /opt/dell
[[ -L /opt/dell/extra ]] || ln -s "$DVD_PATH/extra" /opt/dell/extra
if [[ ! -f /var/log/rubygems-server.log ]]; then
    >/var/log/rubygems-server.log
    chown nobody /var/log/rubygems-server.log
fi

bluepill load /etc/bluepill/rubygems-server.pill
sleep 5

if [[ ! -x /etc/init.d/bluepill ]]; then

    echo "$(date '+%F %T %z'): Installing Chef"
    bring_up_chef || die "Could not start Chef!"

    chef_services=(rabbitmq-server couchdb chef-server chef-server-webui \
        chef-solr chef-expander chef-client)
    # Have Bluepill manage our Chef services instead of letting sysvinit do it.
    echo "$(date '+%F %T %z'): Arranging for Chef to run under Bluepill..."
    for svc in "${chef_services[@]}"; do
        service "$svc" stop || :
    done
    # sometimes couchdb does not die when asked.  Kill it manually.
    if ps aux |grep -q [c]ouchdb; then
        kill $(ps aux |awk '/^couchdb/ {print $2}')
    fi

    # Create an init script for bluepill
    cat > /etc/init.d/bluepill <<EOF
#!/bin/bash
# chkconfig: 2345 90 10
# description: Bluepill Daemon runner
PATH=$PATH
case \$1 in
    start) for pill in /etc/bluepill/*.pill; do
              [[ -f \$pill ]] || continue
              bluepill load "\$pill"
           done;;
    stop) bluepill stop
          bluepill quit;;
    status) if ps aux |grep [b]luepilld; then
             echo "Bluepill is running."
             exit 0
            else
             echo "Bluepill is not running."
             exit 1
            fi;;
    *) echo "\$1: Not supported.";;
esac
EOF

    # enable the bluepill init script and disable the old sysv init scripts.
    if which chkconfig &>/dev/null; then
        chkconfig --add bluepill
        chkconfig bluepill on
        for svc in "${chef_services[@]}"; do
            chkconfig "$svc" off
            chmod ugo-x /etc/init.d/"$svc"
        done
        # to be implemented
    elif which update-rc.d &>/dev/null; then
        update-rc.d bluepill defaults 90 10
        for svc in "${chef_services[@]}"; do
            update-rc.d "$svc" disable
            chmod ugo-x /etc/init.d/"$svc"
        done
    else
        echo "Don't know how to handle services on this system!"
        exit 1
    fi
    # Make sure that the chef log dir has the right permissions
    chown -R chef:chef /var/log/chef
    mkdir -p /var/lib/chef
    chown -R chef:chef /var/lib/chef
    mkdir -p /var/chef
    chown -R chef:chef /var/chef
    bluepill load /etc/bluepill/chef-server.pill
    sleep 30
    chmod 755 /etc/init.d/bluepill
fi

# Bundle up our patches and put them in a sane place
(cd "$DVD_PATH/extra"; tar czf "/tftpboot/patches.tar.gz" patches)

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

# Set the default OS for the provisioner
sed -i "s/%default_os%/$OS_TOKEN/g" \
    /opt/dell/barclamps/provisioner/chef/data_bags/crowbar/bc-template-provisioner.json
if [[ $CROWBAR_REALM && -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
    sed -i -e "s/machine_password/${CROWBAR_KEY##*:}/g" $CROWBAR_FILE
fi

# Crowbar will hack up the pxeboot files appropriatly.
# Set Version in Crowbar UI
sed -i "s/CROWBAR_VERSION = .*/CROWBAR_VERSION = \"${VERSION:=Dev}\"/" \
    /opt/dell/barclamps/crowbar/crowbar_framework/config/environments/production.rb

# Installing Barclamps (uses same library as rake commands, but before rake is ready)

mkdir -p "/opt/dell/bin"
cp "$DVD_PATH/dell/"* /opt/dell/bin
(cd "$DVD_PATH/extra"; cp barclamp* /opt/dell/bin)
chmod 755 /opt/dell/bin/*

# Always run crowbar barclamp first
echo "$(date '+%F %T %z'): Installing Barclamps"
log_to bcinstall /opt/dell/bin/barclamp_install.rb /opt/dell/barclamps/* || \
    die "Could not install barclamps."

echo "$(date '+%F %T %z'): Validating data bags..."
log_to validation validate_bags.rb /opt/dell/chef/data_bags || \
    die "Crowbar configuration has errors.  Please fix and rerun install."

echo "$(date '+%F %T %z'): Create Admin node role"
NODE_ROLE="crowbar-${FQDN//./_}" 
cat > /tmp/role.rb <<EOF
name "$NODE_ROLE"
description "Role for $FQDN"
run_list()
default_attributes( "crowbar" => { "network" => {} } )
override_attributes()
EOF
knifeloop role from file /tmp/role.rb
rm -rf /tmp/role.rb

echo "$(date '+%F %T %z'): Update run list..."
for role in crowbar deployer-client $NODE_ROLE; do
    knifeloop node run_list add "$FQDN" role["$role"] || \
        die "Could not add $role to Chef. Crowbar bringup will fail."
done

pre_crowbar_fixups

echo "$(date '+%F %T %z'): Bringing up Crowbar..."
# Run chef-client to bring-up crowbar server
chef_or_die "Failed to bring up Crowbar"

post_crowbar_fixups

# Wait for puma to start
COUNT=0
while (($COUNT < 60))
do
  sleep 1
  pumactl -S /var/run/crowbar/puma.state stats 2>/dev/null >/devnull && COUNT=60
  COUNT=$(($COUNT + 1))
done

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
# have die change our status to problem if we fail
crowbar_up=true
chef_or_die "Chef run after default proposal commit failed!"

check_machine_role

##
# if we have baked in BMC support, make sure the BMC is responsive.
[[ -f /updates/unbmc.sh ]] && . /updates/unbmc.sh
 
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
    if type -f "transition_check_$state"&>/dev/null; then
        "transition_check_$state" || \
            die "Sanity check for transitioning to $state failed!"
    fi
    chef_or_die "Chef run for $state transition failed!"
    check_machine_role
done

# OK, let looper_chef_client run normally now.

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

update_admin_node
bluepill load /etc/bluepill/chef-client.pill

# transform our friendlier Crowbar default home page.
cd $DVD_PATH/extra
[[ $IP ]] && sed "s@localhost@$IP@g" < index.html.tmpl >/var/www/index.html

if [[ -d /opt/dell/.hooks/admin-post-install.d ]]; then
    local hook
    for hook in /opt/dell/.hooks/admin-post-install.d/*; do
        [[ -x $hook ]] || continue
        $hook || die "Post-install hook ${hook##*/} failed."
    done
fi

echo "Admin node deployed."

# Run tests -- currently the host will run this.
#/opt/dell/bin/barclamp_test.rb -t || \
#    die "Crowbar validation has errors! Please check the logs and correct."
touch /opt/dell/crowbar_framework/.crowbar-installed-ok
