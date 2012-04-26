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
# This script is called by the other install scripts to layout the crowbar
# software + dell pieces.
#
# Requires:
# /tftpboot/ubuntu_dvd is populated with a tarball of the dvd and this file.
#

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
set -x
exec > /root/post-install.log 2>&1

BASEDIR="/tftpboot/ubuntu_dvd"
[[ -f /etc/lsb-release ]] || {
    echo "No lsb-release file, cannot determine proper OS information!"
    exit 1
}
. /etc/lsb-release
OS_TOKEN="ubuntu-${DISTRIB_RELEASE}"

# Make sure /opt is created
mkdir -p /opt/dell/bin

mkdir -p "/tftpboot/$OS_TOKEN"
(cd "/tftpboot/$OS_TOKEN"; ln -s ../ubuntu_dvd install)
echo "deb file:/tftpboot/$OS_TOKEN/install ${DISTRIB_CODENAME} main restricted" \
    > /etc/apt/sources.list

# Make a destination for dell finishing scripts

finishing_scripts=(update_hostname.sh parse_node_data)
( cd "$BASEDIR/dell"; cp "${finishing_scripts[@]}" /opt/dell/bin; )

# "Install h2n for named management"
cd /opt/dell/
tar -zxf "$BASEDIR/extra/h2n.tar.gz"
ln -s /opt/dell/h2n-2.56/h2n /opt/dell/bin/h2n

# Set up initial syslog
cp "$BASEDIR/rsyslog.d/"* /etc/rsyslog.d/

# Barclamp preparation (put them in the right places)
mkdir /opt/dell/barclamps
for i in "$BASEDIR/dell/barclamps/"*".tar.gz"; do
    [[ -f $i ]] || continue
	(cd /opt/dell/barclamps && tar xzf "$i")
	echo "copy new format $i"
done

barclamp_scripts=(barclamp_install.rb barclamp_multi.rb)
( cd "/opt/dell/barclamps/crowbar/bin"; \
    cp "${barclamp_scripts[@]}" /opt/dell/bin; )

# Make sure the bin directory is executable
chmod +x /opt/dell/bin/*

# Make sure we can actaully install Crowbar
chmod +x "$BASEDIR/extra/"*

# Make sure the ownerships are correct
chown -R crowbar.admin /opt/dell

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
            printf "$VAL\n" >>/opt/dell/barclamps/provisioner/chef/cookbooks/provisioner/templates/default/authorized_keys.erb
	    ;;
	crowbar.debug)
	    sed -i -e '/config.log_level/ s/^#//' \
		-e '/config.logger.level/ s/^#//' \
		/opt/dell/barclamps/crowbar/crowbar_framework/config/environments/production.rb
	    ;;

    esac
done

if ! grep -q '192\.168\.124\.10' /etc/network/interfaces; then
    cat >> /etc/network/interfaces <<EOF
auto eth0
iface eth0 inet static
    address 192.168.124.10
    netmask 255.255.255.0
EOF
fi

mkdir -p /opt/dell/bin
ln -s /tftpboot/ubuntu_dvd/extra/install /opt/dell/bin/install-crowbar
