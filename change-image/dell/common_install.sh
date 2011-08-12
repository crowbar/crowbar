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

#
# This script is called by the other install scripts to layout the openstack
# software + dell pieces.
#
# Requires:
# /tftpboot/ubuntu_dvd is populated with a tarball of the dvd and this file.
#

BASEDIR="/tftpboot/ubuntu_dvd"

# explode the dvd.tgz
cd $BASEDIR
if [[ -e $BASEDIR/dvd.tar ]]; then
    tar -xf "$BASEDIR/dvd.tar"
elif [[ -e $BASEDIR/dvd.tgz ]]; then
    tar -zxf "$BASEDIR/dvd.tgz"
fi

# Make sure /opt is created
mkdir -p /opt/dell/bin

# Copy the dell parts into a hidden install directory.
cd /opt
cp -r /$BASEDIR/dell .dell-install

# Make a destination for dell finishing scripts

finishing_scripts=(update_hostname.sh validate_data_bag.rb \
    validate_bags.rb blocking_chef_client.sh looper_chef_client.sh single_chef_client.sh \
    install_barclamp.sh barclamp_lib.rb gather_logs.sh gather_cli.sh)
( cd /opt/.dell-install; cp "${finishing_scripts[@]}" /opt/dell/bin; )

# "Install h2n for named management"
cd /opt/dell/
tar -zxf /tftpboot/ubuntu_dvd/extra/h2n.tar.gz
ln -s /opt/dell/h2n-2.56/h2n /opt/dell/bin/h2n

cp -r /opt/.dell-install/openstack_manager /opt/dell

# Make a destination for switch configs
mkdir -p /opt/dell/switch
cp /opt/.dell-install/*.stk /opt/dell/switch

# Install dell code
cd /opt/.dell-install

# put the chef files in place
cp -r chef /opt/dell
cp rsyslog.d/* /etc/rsyslog.d/

# Install barclamps for now
cd barclamps
for i in *; do
    [[ -d $i ]] || continue
    cd "$i"
    ( cd chef; cp -r * /opt/dell/chef )
    ( cd app; cp -r * /opt/dell/openstack_manager/app )
    ( cd config; cp -r * /opt/dell/openstack_manager/config )
    ( cd command_line; cp * /opt/dell/bin )
    ( cd public ; cp -r * /opt/dell/openstack_manager/public )
    cd ..
done
cd ..

# Make sure the bin directory is executable
chmod +x /opt/dell/bin/*

# Apparmor seems to have something to do with the
# apache hangs.  Disable it for now for testing.
update-rc.d apparmor disable

# put the apt files in place
cp apt.conf sources.list /etc/apt

# Make sure the ownerships are correct
chown -R openstack.admin /opt/dell

#
# Make sure the permissions are right
# Copy from a cd so that means most things are read-only which is fine, except for these.
#
chmod 755 /opt/dell/chef/data_bags/crowbar
chmod 644 /opt/dell/chef/data_bags/crowbar/*
chmod 755 /opt/dell/openstack_manager/db
chmod 644 /opt/dell/openstack_manager/db/*
chmod 755 /opt/dell/openstack_manager/tmp
chmod -R +w /opt/dell/openstack_manager/tmp/*
chmod 755 /opt/dell/openstack_manager/public/stylesheets

# Get out of the directories.
cd 

# Look for any crowbar specific kernel parameters
for s in $(cat /proc/cmdline); do
    VAL=${s#*=} # everything after the first =
    case ${s%%=*} in # everything before the first =
	crowbar.hostname) CHOSTNAME=$VAL;;
	crowbar.url) CURL=$VAL;;
	crowbar.use_serial_console) 
            sed -i "s/\"use_serial_console\": .*,/\"use_serial_console\": $VAL,/" /opt/dell/chef/data_bags/crowbar/bc-template-provisioner.json;;
	crowbar.debug.logdest) 
	    echo "*.*    $VAL" >> /etc/rsyslog.d/00-crowbar-debug.conf
	    mkdir -p "$BASEDIR/rsyslog.d"
	    echo "*.*    $VAL" >> "$BASEDIR/rsyslog.d/00-crowbar-debug.conf"
	    ;;
	crowbar.authkey)
	    mkdir -p "/root/.ssh"
	    printf "$VAL\n" >>/root/.ssh/authorized_keys
	    cp /root/.ssh/authorized_keys "$BASEDIR/authorized_keys"
	    ;;
	crowbar.debug)
	    sed -i -e '/config.log_level/ s/^#//' \
		-e '/config.logger.level/ s/^#//' \
		/opt/dell/openstack_manager/config/environments/production.rb
	    ;;

    esac
done

if [[ $CHOSTNAME ]]; then

    cat > /install_system.sh <<EOF
#!/bin/bash
set -e
cd /tftpboot/ubuntu_dvd/extra
./install $CHOSTNAME

rm -f /etc/rc2.d/S99install
rm -f /etc/rc3.d/S99install
rm -f /etc/rc5.d/S99install

rm -f /install_system.sh

EOF

    chmod +x /install_system.sh
    ln -s /install_system.sh /etc/rc3.d/S99install
    ln -s /install_system.sh /etc/rc5.d/S99install
    ln -s /install_system.sh /etc/rc2.d/S99install
    
fi
