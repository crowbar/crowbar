#!/bin/sh
# This script is called by the net_ubuntu.seed file during
# ubuntu install.  It handles doing the manual sets needed to layout the
# filesystem the rest of the way before running the common_install.sh.
#
# The intent is that once the system is finished installing, the customer
# will edit the config.json file and run install.sh to complete installation
# and initial configuration.
#

#
# We are a net install. 
#

HTTP_SERVER=$1

# Example
#mkdir -p /target/tftpboot/ubuntu_dvd/dell
#wget http://$HTTP_SERVER/ubuntu_dvd/dell/common_install.sh -O /target/tftpboot/ubuntu_dvd/dell/common_install.sh
#wget http://$HTTP_SERVER/ubuntu_dvd/dvd.tgz -O /target/tftpboot/ubuntu_dvd/dvd.tgz
#wget http://$HTTP_SERVER/ubuntu_dvd/jill_ssh.tgz -O /target/tftpboot/ubuntu_dvd/jill_ssh.tgz
#
#chmod +x /target/tftpboot/ubuntu_dvd/dell/common_install.sh
#
#if [ "$2" != "$3" ]
#then
#	chroot /target bash /tftpboot/ubuntu_dvd/dell/common_install.sh fix_if
#else
#	chroot /target bash /tftpboot/ubuntu_dvd/dell/common_install.sh
#fi

rsyslog_dir="/target/etc/rsyslog.d"
if [ -d "$rsyslog_dir" ]; then
    if [ ! -f "$rsyslog_dir/10-crowbar-client.conf" ]; then
        echo "*.* @@${HTTP_SERVER%:*}" > "$rsyslog_dir/10-crowbar-client.conf"
	if ! wget -O "$rsyslog_dir/00-crowbar-debug.conf" -q http://$HTTP_SERVER/ubuntu_dvd/rsyslog.d/00-crowbar-debug.conf
	then
	    rm -f "$rsyslog_dir/00-crowbar-debug.conf"
	fi
    fi
fi
mkdir -p /target/root/.ssh
chmod 700 /target/root/.ssh
if ! wget -O /target/root/.ssh/authorized_keys.wget -q http://$HTTP_SERVER/ubuntu_dvd/authorized_keys; then
    rm -f /target/root/.ssh/authorized_keys.wget
else
    chmod 644 /target/root/.ssh/authorized_keys
    cat /target/root/.ssh/authorized_keys.wget >>/target/root/.ssh/authorized_keys
    rm -f /target/root/.ssh/authorized_keys.wget
fi
sync
sync

