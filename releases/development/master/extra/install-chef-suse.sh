#! /bin/bash -e

# This script is supposed to be run after being installed via the
# crowbar rpm from the SUSE Cloud ISO.  In that context, it is
# expected that all other required repositories (SP2, Updates, etc.)
# are already set up, and a lot of the required files will already be
# in the right place.  However if you want to test setup on a vanilla
# SLES system, you can follow the manual steps below:
#
# 0. export CROWBAR_TESTING=true
# 1. Copy all barclamps to /opt/dell/barclamps
#    You'll want:
#      crowbar database deployer dns glance ipmi keystone logging
#      mysql nagios network nova nova_dashboard ntp openstack postgresql
#      provisioner swift
# 2. Copy extra/barclamp* to /opt/dell/bin/
# 4. You should probably set eth0 to be static IP 192.168.124.10/24.
# 5. rsync the Devel:Cloud, SLES-11-SP2-LATEST, SLE-11-SP2-SDK-LATEST,
#    and possibly other repos into locations under /srv/tftpboot -
#    see https://github.com/SUSE/cloud/wiki/Crowbar for details.

run_succeeded=

exit_handler () {
    if [ -z "$run_succeeded" ]; then
        cat <<EOF

Crowbar installation terminated prematurely.  Please examine the
above output for clues as to what went wrong.  You should also
check the SUSE Cloud Installation Manual, in particular the
Troubleshooting section.  Note that this script can safely be
re-run multiple times if required.
EOF
    fi
}

trap exit_handler EXIT

die() { echo "$(date '+%F %T %z'): $*" >&2; res=1; exit 1; }

ensure_service_running () {
    service="$1"
    regexp="${2:-running}"
    if service $service status | egrep -q "$regexp"; then
        echo "$service is already running - no need to start."
    else
        service $service start
    fi
}

# It is exceedingly important that 'hostname -f' actually returns an FQDN!
# if it doesn't, add an entry to /etc/hosts, e.g.:
#    192.168.124.10 cb-admin.example.com cb-admin
if ! FQDN=$(hostname -f 2>/dev/null); then
    die "Unable to detect fully-qualified hostname. Aborting."
fi

if ! DOMAIN=$(hostname -d 2>/dev/null); then
    die "Unable to detect DNS domain name. Aborting."
fi
 
if [ -z "$FQDN" -o -z "$DOMAIN" ]; then
    die "Unable to detect fully-qualified hostname. Aborting."
fi

if ! resolved=$(getent ahosts $FQDN 2>/dev/null); then
    die "Unable to resolve hostname $FQDN via host(1). Please check your configuration of DNS, hostname, and /etc/hosts. Aborting."
fi

IPv4_addr=$( echo "$resolved" | awk '{ if ($1 !~ /:/) { print $1; exit } }' )
IPv6_addr=$( echo "$resolved" | awk '{ if ($1  ~ /:/) { print $1; exit } }' )
if [ -z "$IPv4_addr" -a -z "$IPv6_addr" ]; then
    die "Could not resolve $FQDN to an IPv4 or IPv6 address. Aborting."
fi

if [ -n "$IPv4_addr" ]; then
    echo "$FQDN resolved to IPv4 address: $IPv4_addr"
    if ! ip addr | grep -q "inet $IPv4_addr"; then
        die "No local interfaces configured with address $IPv4_addr. Aborting."
    fi
    if [[ "$IPv4_addr" =~ ^127 ]]; then
        die "$FQDN resolves to a loopback address. Aborting."
    fi
fi
if [ -n "$IPv6_addr" ]; then
    echo "$FQDN resolved to IPv6 address: $IPv6_addr"
    if ! ip addr | grep -q "inet6 $IPv6_addr"; then
        die "No local interfaces configured with address $IPv6_addr. Aborting."
    fi
fi

if ! ping -c 1 $FQDN >/dev/null 2>&1; then
    die "Failed to ping $FQDN; please check your network configuration. Aborting."
fi


# output details, that will make remote debugging via bugzilla much easier  
# for us
/usr/bin/zypper lr -d   
/bin/rpm -qV crowbar || :
/usr/bin/lscpu  
/bin/df -h  
/usr/bin/free -m
/bin/ls -la /srv/tftpboot/repos/ /srv/tftpboot/repos/Cloud/ /srv/tftpboot/suse-11.2/install/
/usr/bin/grep media_url /opt/dell/chef/cookbooks/provisioner/templates/default/autoyast.xml.erb

CROWBAR=/opt/dell/bin/crowbar

for repo in suse-11.2/install repos/Cloud; do
    repo=/srv/tftpboot/$repo
    if [ "$( ls $repo 2>/dev/null | wc -l )" = 0 ]; then
        if [ -n "$CROWBAR_TESTING" ]; then
            die "$repo has not been set up yet; please see https://github.com/SUSE/cloud/wiki/Crowbar"
        else
            die "$repo has not been set up yet; please check you didn't miss a step in the installation guide."
        fi
    fi
done

if [ -n "$CROWBAR_TESTING" ]; then
    # This is supposed to go away once the Chef dependencies are included in the
    # add-on image.  Note that SP1 is required for rubygem-haml at least, but
    # the new maintenance model requires SP1 repos alongside SP2 anyway.
    zypper ar    http://dist.suse.de/install/SLP/SLES-11-SP2-GM/x86_64/DVD1 sp2
    zypper ar    http://dist.suse.de/install/SLP/SLE-11-SP2-SDK-GM/x86_64/DVD1/ sdk-sp2
    zypper ar    http://dist.suse.de/ibs/SUSE:/SLE-11-SP1:/GA/standard/ sp1-ga
    zypper ar    http://dist.suse.de/ibs/SUSE:/SLE-11-SP2:/GA/standard/ sp2-ga
    zypper ar -f http://dist.suse.de/ibs/SUSE:/SLE-11-SP1:/Update/standard/ sp1-update
    zypper ar -f http://dist.suse.de/ibs/SUSE:/SLE-11-SP2:/Update/standard/ sp2-update
    zypper ar -f http://dist.suse.de/ibs/Devel:/Cloud/SLE_11_SP2/ cloud

    # install chef and its dependencies
    zypper --gpg-auto-import-keys in rubygem-chef-server rubygem-chef rabbitmq-server couchdb java-1_6_0-ibm rubygem-activesupport efibootmgr

    # also need these (crowbar dependencies):
    zypper in rubygem-kwalify rubygem-ruby-shadow tcpdump

    # Need this for provisioner to work:
    mkdir -p /srv/tftpboot/discovery/pxelinux.cfg
    cat > /srv/tftpboot/discovery/pxelinux.cfg/default <<EOF
DEFAULT pxeboot
TIMEOUT 20
PROMPT 0
LABEL pxeboot
        KERNEL vmlinuz0
        APPEND initrd=initrd0.img root=/sledgehammer.iso rootfstype=iso9660 rootflags=loop
ONERROR LOCALBOOT 0
EOF

    # You'll also need:
    #   /srv/tftpboot/discovery/initrd0.img
    #   /srv/tftpboot/discovery/vmlinuz0
    # These can be obtained from a sleshammer image or from an existing
    # ubuntu admin node.
fi

chkconfig rabbitmq-server on
ensure_service_running rabbitmq-server '^Node .+ with Pid [0-9]+: running'

if rabbitmqctl list_vhosts | grep -q '^/chef$'; then
    : /chef vhost already added
else
    rabbitmqctl add_vhost /chef
fi

if rabbitmqctl list_users 2>&1 | grep -q '^chef	'; then
    : chef user already added
else
    rabbit_chef_password=$( dd if=/dev/urandom count=1 bs=16 2>/dev/null | base64 | tr -d / )
    rabbitmqctl add_user chef "$rabbit_chef_password"
    # Update "amqp_pass" in  /etc/chef/server.rb and solr.rb
    sed -i 's/amqp_pass ".*"/amqp_pass "'"$rabbit_chef_password"'"/' /etc/chef/{server,solr}.rb
fi

sed -i 's/web_ui_admin_default_password ".*"/web_ui_admin_default_password "password"/' /etc/chef/webui.rb
chmod o-rwx /etc/chef /etc/chef/{server,solr,webui}.rb

rabbitmqctl set_permissions -p /chef chef ".*" ".*" ".*"

chkconfig couchdb on
ensure_service_running couchdb

# increase chef-solr index field size
perl -i -pe 's{<maxFieldLength>.*</maxFieldLength>}{<maxFieldLength>200000</maxFieldLength>}' /var/lib/chef/solr/conf/solrconfig.xml

services='solr expander server server-webui'
for service in $services; do
    chkconfig chef-${service} on
done

for service in $services; do
    ensure_service_running chef-${service}
done

# chef-server-webui won't start if /etc/chef/webui.pem doesn't exist, which
# may be the case (chef-server generates it if not present, and if the webui
# starts too soon, it won't be there yet).
for ((x=1; x<6; x++)); do
    sleep 10
    service chef-server-webui status >/dev/null && { chef_webui_running=true; break; }
    service chef-server-webui start
done
if [[ ! $chef_webui_running ]]; then
    echo "WARNING: unable to start chef-server-webui"
fi

if ! [ -e ~/.chef/knife.rb ]; then
    yes '' | knife configure -i
fi

node_info=$(knife node show $FQDN 2>/dev/null || :)
if echo "$node_info" | grep -q 'Run List:.*role'; then
    echo "Chef runlist for $FQDN is already populated; skipping initial chef-client run."
else
    cat <<EOF
Performing initial chef-client run ...
This can cause warnings about /etc/chef/client.rb missing and
the run list being empty; they can be safely ignored.
EOF
    chef-client
fi

# Don't use this one - crowbar barfs due to hyphens in the "id" attribute.
#CROWBAR_FILE="/opt/dell/barclamps/crowbar/chef/data_bags/crowbar/bc-template-crowbar.json"
: ${CROWBAR_FILE:="/etc/crowbar/crowbar.json"}

mkdir -p /opt/dell/crowbar_framework
CROWBAR_REALM=$(/opt/dell/barclamps/provisioner/updates/parse_node_data $CROWBAR_FILE -a attributes.crowbar.realm)
CROWBAR_REALM=${CROWBAR_REALM##*=}

# Generate the machine install username and password.
if [[ ! -e /etc/crowbar.install.key && $CROWBAR_REALM ]]; then
    dd if=/dev/urandom bs=65536 count=1 2>/dev/null |sha512sum - 2>/dev/null | \
        (read key rest; echo "machine-install:$key" >/etc/crowbar.install.key)
fi

if [[ $CROWBAR_REALM && -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
    sed -i -e "s/machine_password/${CROWBAR_KEY##*:}/g" $CROWBAR_FILE
fi
# Set the default OS for the provisioner
sed -i 's/%default_os%/suse-11.2/g' \
    /opt/dell/barclamps/provisioner/chef/data_bags/crowbar/bc-template-provisioner.json


/opt/dell/bin/barclamp_install.rb /opt/dell/barclamps/crowbar

#
# Take care that the barclamps are installed in the right order
# If you've got a full openstack set installed, e.g.: nagios has to be
# installed before keystone, postgresql and mysql has to be installed
# before database, etc.
#
for i in deployer dns mysql postgresql database ipmi nagios keystone \
         glance logging network nova nova_dashboard ntp openstack \
         provisioner swift ; do
    if [ -e /opt/dell/crowbar_framework/barclamps/$i.yml ]; then
        echo "$i barclamp is already installed"
    else
        /opt/dell/bin/barclamp_install.rb /opt/dell/barclamps/$i
    fi
done

# Configure chef to set up bind with correct local domain and DNS forwarders.
dns_template=/opt/dell/chef/data_bags/crowbar/bc-template-dns.json
[ -f $dns_template ] || die "$dns_template doesn't exist"
nameservers=$( awk '/^nameserver/ {print $2}' /etc/resolv.conf )
# This will still work if there are no nameservers.
/opt/dell/bin/bc-dns-json.rb $DOMAIN $nameservers < $dns_template > /tmp/bc-template-dns.json
echo "Instructing chef to configure bind with the following DNS forwarders: $nameservers"
knife data bag from file crowbar /tmp/bc-template-dns.json

echo "Create Admin node role"
NODE_ROLE="crowbar-${FQDN//./_}" 
cat > /tmp/role.rb <<EOF
name "$NODE_ROLE"
description "Role for $FQDN"
run_list()
default_attributes( "crowbar" => { "network" => {} } )
override_attributes()
EOF
knife role from file /tmp/role.rb

knife node run_list add "$FQDN" role["crowbar"]
knife node run_list add "$FQDN" role["deployer-client"]
knife node run_list add "$FQDN" role["$NODE_ROLE"]

# at this point you can run chef-client from the command line to start
# the crowbar bootstrapping

chef-client

# OOC, what, if anything, is responsible for starting rainbows/crowbar under bluepill?
ensure_service_running crowbar

# Make sure looper_chef_client is a NOOP until we are finished deploying
touch /tmp/deploying
# This works because
#
#   crowbar_framework/app/models/provisioner_service.rb
#   crowbar_framework/app/models/service_object.rb
#
# both invoke
#
#   /opt/dell/bin/single_chef_client.sh (from barclamps/crowbar/bin)
#
# which invokes
#
#   /opt/dell/bin/looper_chef_client.sh
#
# which exits immediately if /tmp/deploying exists.

# From here, you should probably read along with the equivalent steps in
# install-chef.sh for comparison

if [ "$($CROWBAR crowbar proposal list)" != "default" ] ; then
    proposal_opts=()
    # If your custom crowbar.json is somewhere else, probably substitute that here
    if [[ -e $CROWBAR_FILE ]]; then
        proposal_opts+=(--file $CROWBAR_FILE)
    fi
    proposal_opts+=(proposal create default)

    # Sometimes proposal creation fails if Chef and Crowbar are not quite
    # fully prepared -- this can happen due to solr not having everything
    # fully indexed yet.  So we don't want to just fail immediatly if
    # we fail to create a proposal -- instead, we will kick Chef, sleep a bit,
    # and try again up to 5 times before bailing out.
    for ((x=1; x<6; x++)); do
        $CROWBAR crowbar "${proposal_opts[@]}" && { proposal_created=true; break; }
        echo "Proposal create failed, pass $x.  Will kick Chef and try again."
        chef-client
        sleep 1
    done
    if [[ ! $proposal_created ]]; then
        die "Could not create default proposal"
    fi
fi

# this has machine key world readable? care?
$CROWBAR crowbar proposal show default >/var/log/default-proposal.json

# next will fail if ntp barclamp not present (or did for me...)
$CROWBAR crowbar proposal commit default || \
    die "Could not commit default proposal!"
    
$CROWBAR crowbar show default >/var/log/default.json

crowbar_up=true
chef-client

# here we whould check indexer/expander is finished

# original script has several calls to check_machine_role -- see source for
# this, in my limited testing it wasn't necessary on SUSE, but we should still
# do it anyway (if the role isn't present things break, apparently)

# BMC support?

# transition though all the states to ready.  Make sure that
# Chef has completly finished with transition before proceeding
# to the next.

for state in "discovering" "discovered" "hardware-installing" \
    "hardware-installed" "installing" "installed" "readying" "ready"
do
    while [[ -f "/tmp/chef-client.lock" ]]; do sleep 1; done
    printf "$state: "
    $CROWBAR crowbar transition "$FQDN" "$state" || \
        die "Transition to $state failed!"
    if type -f "transition_check_$state"&>/dev/null; then
        "transition_check_$state" || \
            die "Sanity check for transitioning to $state failed!"
    fi
    # chef_or_die "Chef run for $state transition failed!"
    chef-client
    # check_machine_role
done

# OK, let looper_chef_client run normally now.
rm /tmp/deploying

# Need chef-client daemon now
ensure_service_running chef-client

# Spit out a warning message if we managed to not get an IP address
IPSTR=$($CROWBAR network show default | /opt/dell/barclamps/provisioner/updates/parse_node_data -a attributes.network.networks.admin.ranges.admin.start)
IP=${IPSTR##*=}
ip addr | grep -q $IP || {
    die "eth0 not configured, but should have been."
}

# Run tests -- currently the host will run this.
# 2012-05-24: test is failing, so only run if CROWBAR_RUN_TESTS=true
# (this is distinct from CROWBAR_TESTING, which would add extra repos etc.)
if [ -n "$CROWBAR_RUN_TESTS" ]; then
    /opt/dell/bin/barclamp_test.rb -t || \
        die "Crowbar validation has errors! Please check the logs and correct."
fi

touch /opt/dell/crowbar_framework/.crowbar-installed-ok

cat <<EOF
Admin node deployed.

You can now visit the Crowbar web UI on http://$IP:3000/
and the Chef web UI on http://$IP:4040/

You should also now be able to PXE-boot a client.

Note that to run the crowbar CLI tool, you will need to log out
and log back in again for the correct environment variables to
be set up.
EOF

run_succeeded=hooray
