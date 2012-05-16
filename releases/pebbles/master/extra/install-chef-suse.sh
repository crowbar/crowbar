#! /bin/bash

# NOTE: Actually running this script has not been tested!  At this stage you
# might be best off just copy & pasting it into an interactive shell line by
# line to have the full experience.
# -- tserong 2011-05-14

# 1. Copy all barclamps to /opt/dell/barclamps
#    You'll want:
#      crowbar database deployer dns glance ipmi keystone logging
#      mysql nagios network nova nova_dashboard ntp openstack postgresql
#      provisioner swift
# 2. Copy extra/barclamp* to /opt/dell/bin/
# 3. Prepend /opt/dell/bin to $PATH (else crowbar command won't be found)
# 4. You should probably set eth0 to be static IP 192.168.124.10/24.

die() { echo "$(date '+%F %T %z'): $*" >&2; res=1; exit 1; }

# It is exceedingly important that 'hostname -f' actually returns an FQDN!
# if it doesn't, add an entry to /etc/hosts, e.g.:
#    192.168.124.10 cb-admin.example.com cb-admin
FQDN=$(hostname -f 2> /dev/null)
if [ $? != 0 ]; then
    die "Unable to resolve hostname. Exiting."
fi

DOMAIN=$(hostname -d 2> /dev/null)
if [ $? != 0 ]; then
    die "Unable to resolve domain name. Exiting."
fi

# This is supposed to go away once the Chef dependencies are included in the
# addon image
zypper ar -f http://dist.suse.de/ibs/SUSE:/SLE-11-SP1:/GA/standard/ sp1-ga
zypper ar -f http://dist.suse.de/ibs/SUSE:/SLE-11-SP1:/Update/standard/ sp1-update
zypper ar -f http://dist.suse.de/ibs/Devel:/Cloud/SLE_11_SP2/ cloud
zypper ar -f http://dist.suse.de/install/SLP/SLE-11-SP2-SDK-GM/x86_64/DVD1/ sdk

# install chef and its dependencies
zypper --gpg-auto-import-keys in rubygem-chef-server rubygem-chef rabbitmq-server couchdb java-1_6_0-ibm rubygem-activesupport

# also need these (crowbar dependencies):
zypper in rubygem-kwalify rubygem-ruby-shadow tcpdump

# couchdb needs this (it might not be present if you start from a JeOS image)
zypper in sudo

# Need this for provisioner to work:
mkdir -p /tftpboot/discovery/pxelinux.cfg
cat > /tftpboot/discovery/pxelinux.cfg/default <<EOF
DEFAULT pxeboot
TIMEOUT 20
PROMPT 0
LABEL pxeboot
        KERNEL vmlinuz0
        APPEND initrd=initrd0.img root=/sledgehammer.iso rootfstype=iso9660 rootflags=loop
ONERROR LOCALBOOT 0
EOF

# You'll also need:
#   /tftpboot/discovery/initrd0.img
#   /tftpboot/discovery/vmlinuz0
# These can be obtained from the existing ubuntu admin node


# setup rabbitmq
chkconfig rabbitmq-server on
service rabbitmq-server start

rabbitmqctl add_vhost /chef

rabbit_chef_password=$( dd if=/dev/random count=1 bs=16 2>/dev/null | base64 | tr -d / )
rabbitmqctl add_user chef "$rabbit_chef_password"

rabbitmqctl set_permissions -p /chef chef ".*" ".*" ".*"

# setup couchdb
chkconfig couchdb on
service couchdb start

# Update "amqp_pass" in  /etc/chef/server.rb and solr.rb
sed -i 's/amqp_pass ".*"/amqp_pass "'"$rabbit_chef_password"'"/' /etc/chef/{server,solr}.rb
sed -i 's/web_ui_admin_default_password ".*"/web_ui_admin_default_password "password"/' /etc/chef/webui.rb
chmod o-rwx /etc/chef /etc/chef/{server,solr,webui}.rb

# increase chef-solr index field size
perl -i -ne 'if ($_ =~ /<maxFieldLength>(.*)<\/maxFieldLength>/){ print "<maxFieldLength>200000</maxFieldLength> \n" } else { print } '  /var/lib/chef/solr/conf/solrconfig.xml

services='server server-webui solr expander'
for service in $services; do
    chkconfig chef-${service} on
done

for service in $services; do
    service chef-${service} start
done

#initial chef-client run
chef-client

# now set the correct domain name in /opt/dell/barclamps/dns/chef/data_bags/crowbar/bc-template-dns.json
if [ -f /opt/dell/barclamps/dns/chef/data_bags/crowbar/bc-template-dns.json ]; then
    sed -i "s/^\(\s*\"domain\"\s*\:\s*\)\".*\"/\1\"$DOMAIN\"/" \
        /opt/dell/barclamps/dns/chef/data_bags/crowbar/bc-template-dns.json
else
    echo "/opt/dell/barclamps/dns/chef/data_bags/crowbar/bc-template-dns.json doesn't exist"
fi

# Also, create a crowbar.json somewhere (/root/crowbar.json, or
# $DVD_PATH/extra/config/crowbar.json).  This file is from the 
# root directory of the crowbar github repo.  Remove nagios and
# ganglia (until we decide what to do with them), then set
# $CROWBAR_FILE to point to this file.

# generate the machine install username and password
CROWBAR_FILE="/opt/dell/barclamps/crowbar/chef/data_bags/crowbar/bc-template-crowbar.json"
if [[ -e $DVD_PATH/extra/config/crowbar.json ]]; then
  CROWBAR_FILE="$DVD_PATH/extra/config/crowbar.json"
fi
mkdir -p /opt/dell/crowbar_framework
CROWBAR_REALM=$(/opt/dell/barclamps/provisioner/updates/parse_node_data $CROWBAR_FILE -a attributes.crowbar.realm)
CROWBAR_REALM=${CROWBAR_REALM##*=}
if [[ ! -e /etc/crowbar.install.key && $CROWBAR_REALM ]]; then
    dd if=/dev/urandom bs=65536 count=1 2>/dev/null |sha512sum - 2>/dev/null | \
        (read key rest; echo "machine-install:$key" >/etc/crowbar.install.key)
fi

if [[ $CROWBAR_REALM && -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
    sed -i -e "s/machine_password/${CROWBAR_KEY##*:}/g" $CROWBAR_FILE
fi

/opt/dell/bin/barclamp_install.rb /opt/dell/barclamps/crowbar
# Note you may have to do some of the following barclamps manually if you've
# got a full openstack set installed, e.g.: nagios has to be installed before
# keystone, postgresql has to be installed before database, etc.
/opt/dell/bin/barclamp_install.rb /opt/dell/barclamps/*

knife configure -i

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
service crowbar start

# what's this for?
touch /tmp/deploying

# from here, you should probably read along with the equivalent steps in
# install-chef.sh for comparison

if [ "$(crowbar crowbar proposal list)" != "default" ] ; then
    proposal_opts=()
    # If your custom crowbar.json is somewhere else, probably substitute that here
    if [[ -e $DVD_PATH/extra/config/crowbar.json ]]; then
        proposal_opts+=(--file $DVD_PATH/extra/config/crowbar.json)
    fi
    proposal_opts+=(proposal create default)
    crowbar crowbar "${proposal_opts[@]}"
    chef-client
    # Note; original script loops here and dies on failure
fi

# this has machine key world readable? care?
crowbar crowbar proposal show default >/var/log/default-proposal.json

# next will fail if ntp barclamp not present (or did for me...)
crowbar crowbar proposal commit default || \
    die "Could not commit default proposal!"
    
crowbar crowbar show default >/var/log/default.json

crowbar_up = true
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
    crowbar crowbar transition "$FQDN" "$state" || \
        die "Transition to $state failed!"
    if type -f "transition_check_$state"&>/dev/null; then
        "transition_check_$state" || \
            die "Sanity check for transitioning to $state failed!"
    fi
    # chef_or_die "Chef run for $state transition failed!"
    chef-client
    # check_machine_role
done

# missing check for admin node IP address, probably need to start 
# chef-client daemon too.

rm /tmp/deploying

echo "Admin node deployed."

# missing tests here

# now, if you PXE boot a client, it "should just work".  Note that I had
# some trouble with the sledgehammer image nfs mounting the admin node
# (which is a rather non-obvious failure).  Restarting nfsserver on the
# admin node seemed to fix it -- possibly the provisioner barclamp still
# needs some work in that regard -- tserong
