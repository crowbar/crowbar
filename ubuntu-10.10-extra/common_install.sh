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

# Build our package cache for package installs.
(   cd "$BASEDIR/extra"
    [[ -d pkgs ]] && dpkg-scanpackages pkgs /dev/null 2>/dev/null | \
	gzip -9 >Packages.gz 
)

# Copy the dell parts into a hidden install directory.
cd /opt
cp -r /$BASEDIR/dell .dell-install

# Make a destination for dell finishing scripts

finishing_scripts=(update_hostname.sh parse_node_data)
( cd /opt/.dell-install; cp "${finishing_scripts[@]}" /opt/dell/bin; )

barclamp_scripts=(barclamp_install.rb)
( cd /opt/.dell-install/barclamps/crowbar/bin; cp "${barclamp_scripts[@]}" /opt/dell/bin; )

# "Install h2n for named management"
cd /opt/dell/
tar -zxf /tftpboot/ubuntu_dvd/extra/h2n.tar.gz
ln -s /opt/dell/h2n-2.56/h2n /opt/dell/bin/h2n

# Install dell code
cd /opt/.dell-install

# Set up initial syslog
cp rsyslog.d/* /etc/rsyslog.d/

# Barclamp preparation (put them in the right places)
mkdir /opt/dell/barclamps
cd barclamps
for i in *; do
    [[ -d $i ]] || continue
    if [ -e $i/crowbar.yml ]; then
      # MODULAR FORMAT copy to right location (installed by rake barclamp:install)
      cp -r $i /opt/dell/barclamps
      echo "copy new format $i"
    else
      echo "WARNING: item $i found in barclamp directory, but it is not a barclamp!"
    fi 
done
cd ..

# Make sure the bin directory is executable
chmod +x /opt/dell/bin/*

# Make sure the ownerships are correct
chown -R crowbar.admin /opt/dell

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
