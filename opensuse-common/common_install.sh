#!/bin/bash

(cd /etc/zypp/repos.d && rm *)

(   mkdir -p "/srv/tftpboot/$OS_TOKEN"
    cd "/srv/tftpboot/$OS_TOKEN"
    ln -s ../opensuse_dvd install)

REPO_URL="file:///srv/tftpboot/$OS_TOKEN/install/suse"
cat >"/etc/zypp/repos.d/$OS_TOKEN-Base.repo" <<EOF
[$OS_TOKEN-Base]
name=$OS_TOKEN Base
baseurl=$REPO_URL
gpgcheck=0
EOF

# Barclamp preparation (put them in the right places)
mkdir -p /opt/dell/barclamps
for i in "$BASEDIR/dell/barclamps/"*".tar.gz"; do
    [[ -f $i ]] || continue
    ( cd "/opt/dell/barclamps"; tar xzf "$i"; )
done

find /opt/dell/barclamps -type d -name cache -maxdepth 2 | while read src; do
    [[ -d $src/$OS_TOKEN/pkgs/repodata ]] || continue
    bc=${src%/cache}
    bc=${bc##*/}
   cat >"/etc/zypp/repos.d/crowbar-$bc.repo" <<EOF
[crowbar-$bc]
name=Crowbar $bc Packages
baseurl=file://$src/$OS_TOKEN/pkgs
gpgcheck=0
EOF
done

for bc in "$BASEDIR/dell/barclamps/"*.rpm; do
    [[ -f $bc ]] || continue
    mkdir -p /opt/dell/rpms
    cp "$bc" /opt/dell/rpms
done
if [[ -d /opt/dell/rpms ]]; then
#    (cd /opt/dell/rpms; createrepo -d -q .)
    cat >"/etc/zypp/repos.d/crowbar.repo" <<EOF
[crowbar]
name=Crowbar Packages
baseurl=file:///opt/dell/rpms
gpgcheck=0
EOF
fi

# Make runlevel 3 the default
sed -i -e '/^id/ s/5/3/' /etc/inittab

# We prefer rsyslog.
zypper install -l -f -n -y rsyslog
systemctl enable rsyslog

# put the chef files in place
cp "$BASEDIR/rsyslog.d/"* /etc/rsyslog.d/

# Restart rsyslog to pick up out changes
rcsyslog restart

# Make sure /opt is created
mkdir -p /opt/dell/bin

# Make a destination for dell finishing scripts

finishing_scripts=(update_hostname.sh parse_node_data)
( cd "$BASEDIR/dell"; cp "${finishing_scripts[@]}" /opt/dell/bin; )

barclamp_scripts=(barclamp_install.rb barclamp_multi.rb)
( cd "/opt/dell/barclamps/crowbar/bin" &&  \
    cp "${barclamp_scripts[@]}" /opt/dell/bin || :)

# Make sure the bin directory is executable
chmod +x /opt/dell/bin/*

# Make sure we can actaully install Crowbar
chmod +x "$BASEDIR/extra/"*

# This directory is the model to help users create new barclamps
cp -r /opt/dell/barclamps/crowbar/crowbar_framework/barclamp_model /opt/dell || :

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
            printf "$VAL\n" | sed 's/040/ /g' >>/root/.ssh/authorized_keys
            printf "$VAL\n" >>/opt/dell/barclamps/provisioner/chef/cookbooks/provisioner/templates/default/authorized_keys.erb
            ;;
        crowbar.debug)
            sed -i -e '/config.log_level/ s/^#//' \
                -e '/config.logger.level/ s/^#//' \
                /opt/dell/barclamps/crowbar/crowbar_framework/config/environments/production.rb
            ;;
    esac
done

ln -s /srv/tftpboot/opensuse_dvd/extra/install /opt/dell/bin/install-crowbar
echo "PermitRootLogin yes" >>/etc/ssh/sshd_config

exit 0
