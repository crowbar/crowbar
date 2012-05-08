#! /bin/bash

# 1. Copy all barclamps to /opt/dell/barclamps
# 2. Copy extra/barclamp* to /opt/dell/bin/

# This is suppose to go way once the Chef dependencies are included in the
# addon image
zypper ar -f http://dist.suse.de/ibs/SUSE:/SLE-11-SP1:/GA/standard/ sp1-ga
zypper ar -f http://dist.suse.de/ibs/Devel:/Cloud/SLE_11_SP2/ cloud
zypper ar -f http://dist.suse.de/install/SLP/SLE-11-SP2-SDK-GM/x86_64/DVD1/ sdk

# install chef and its dependencies
zypper in rubygem-chef-server rubygem-chef rabbitmq-server couchdb java-1_6_0-ibm rubygem-activesupport


FQDN=$(hostname -f)
# setup rabbitmq
chkconfig rabbitmq-server on
service rabbitmq-server start

rabbitmqctl add_vhost /chef

# FIXME Hm, do we need a secure default password?
rabbitmqctl add_user chef testing

rabbitmqctl set_permissions -p /chef chef ".*" ".*" ".*"

# setup couchdb
chkconfig couchdb on
service couchdb start

# Update "amqp_pass" in  /etc/chef/server.rb and solr.rb
sed -i 's/amqp_pass ".*"/amqp_pass "testing"/' /etc/chef/server.rb
sed -i 's/amqp_pass ".*"/amqp_pass "testing"/' /etc/chef/solr.rb
sed -i 's/web_ui_admin_default_password ".*"/web_ui_admin_default_password "password"/' /etc/chef/webui.rb
# increase chef-solr index field size
perl -i -ne 'if ($_ =~ /<maxFieldLength>(.*)<\/maxFieldLength>/){ print "<maxFieldLength>200000</maxFieldLength> \n" } else { print } '  /var/lib/chef/solr/conf/solrconfig.xml


for svc in server server-webui solr expander do
    chkconfig chef-${svc} on
done

for svc in server server-webui solr expander do
    service chef-${svc} start
done

#initial chef-client run
chef-client

# now set the correct domain name in /opt/dell/barclamps/dns/chef/data_bags/crowbar/bc-template-dns.json



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
knifeloop role from file /tmp/role.rb

knife node run_list add "$FQDN" role["crowbar"]
knife node run_list add "$FQDN" role["deployer-client"]
knife node run_list add "$FQDN" role["crowbar-admin_crowbar_site"]

