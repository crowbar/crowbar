#!/bin/bash
cat <<EOF >/etc/sysconfig/network-scripts/ifcfg-eth0
DEVICE=eth0
BOOTPROTO=none
ONBOOT=yes
NETMASK=255.255.255.0
IPADDR=192.168.124.10
GATEWAY=192.168.124.1
TYPE=Ethernet
EOF

(cd /etc/yum.repos.d && rm *)

(   mkdir -p "/tftpboot/$OS_TOKEN"
    cd "/tftpboot/$OS_TOKEN"
    ln -s ../redhat_dvd install)

REPO_URL="file:///tftpboot/$OS_TOKEN/install/Server"
[[ -d tftpboot/$OS_TOKEN/install/repodata ]] && \
    REPO_URL="file:///tftpboot/$OS_TOKEN/install"

cat >"/etc/yum.repos.d/$OS_TOKEN-Base.repo" <<EOF
[$OS_TOKEN-Base]
name=$OS_TOKEN Base
baseurl=$REPO_URL
gpgcheck=0
EOF

# for CentOS.
(cd "$BASEDIR"; [[ -d Server ]] || ln -sf . Server)

# We prefer rsyslog.
yum -y install rsyslog
chkconfig syslog off
chkconfig rsyslog on

# Make sure rsyslog picks up our stuff
echo '$IncludeConfig /etc/rsyslog.d/*.conf' >>/etc/rsyslog.conf
mkdir -p /etc/rsyslog.d/

# Make runlevel 3 the default
sed -i -e '/^id/ s/5/3/' /etc/inittab

# Make sure /opt is created
mkdir -p /opt/dell/bin

# Make a destination for dell finishing scripts

finishing_scripts=(update_hostname.sh parse_node_data)
( cd "$BASEDIR/dell"; cp "${finishing_scripts[@]}" /opt/dell/bin; )

# "Install h2n for named management"
cd /opt/dell/
tar -zxf "$BASEDIR/extra/h2n.tar.gz"
ln -s /opt/dell/h2n-2.56/h2n /opt/dell/bin/h2n

# put the chef files in place
cp "$BASEDIR/rsyslog.d/"* /etc/rsyslog.d/

# Barclamp preparation (put them in the right places)
mkdir /opt/dell/barclamps
for i in "$BASEDIR/dell/barclamps/"*".tar.gz"; do
    [[ -f $i ]] || continue
    ( cd "/opt/dell/barclamps"; tar xzf "$i"; )
done

barclamp_scripts=(barclamp_install.rb barclamp_multi.rb)
( cd "/opt/dell/barclamps/crowbar/bin"; \
    cp "${barclamp_scripts[@]}" /opt/dell/bin; )

# Make sure the bin directory is executable
chmod +x /opt/dell/bin/*

# Make sure we can actaully install Crowbar
chmod +x "$BASEDIR/extra/"*

# This directory is the model to help users create new barclamps
cp -r /opt/dell/barclamps/crowbar/crowbar_framework/barclamp_model /opt/dell

# "Blacklisting IPv6".
echo "blacklist ipv6" >>/etc/modprobe.d/blacklist-ipv6.conf
echo "options ipv6 disable=1" >>/etc/modprobe.d/blacklist-ipv6.conf

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

mkdir -p /opt/dell/bin
ln -s /tftpboot/redhat_dvd/extra/install /opt/dell/bin/install-crowbar
