#!/bin/bash

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
exec > /root/post-install.log 2>&1

BASEDIR="/tftpboot/ubuntu_dvd"
# copy the install image.
mkdir -p "$BASEDIR"
(   cd "$BASEDIR"
    while ! wget -q http://192.168.1.2:8091/files.list; do sleep 1; done
    while read f; do
	wget -a /root/post-install-wget.log -x -nH --cut-dirs=1 \
	    "http://192.168.1.2:8091/${f#./}"
    done < files.list
    rm files.list
)

# Make runlevel 3 the default
sed -i -e '/^id/ s/5/3/' /etc/inittab

mdcp() {
    local dest="$1"
    shift
    mkdir -p "$dest"
    cp "$@" "$dest"
}

# Build our package cache for package installs.
(   cd "$BASEDIR/extra"
    [[ -d pkgs ]] && dpkg-scanpackages pkgs /dev/null 2>/dev/null | \
        gzip -9 >Packages.gz
)

finishing_scripts="update_hostname.sh parse_node_data"
(
    cd "$BASEDIR/dell"
    mdcp /opt/dell/bin $finishing_scripts
)

barclamp_scripts="barclamp_install.rb"
( 
    cd $BASEDIR/dell/barclamps/crowbar/bin
    mdcp /opt/dell/bin $barclamp_scripts
)

# Install h2n for named management
( 
    cd /opt/dell/; 
    tar -zxf ${BASEDIR}/extra/h2n.tar.gz
)
ln -s /opt/dell/h2n-2.56/h2n /opt/dell/bin/h2n    
    

# put the chef files in place
mdcp /etc/rsyslog.d "$BASEDIR/dell/rsyslog.d/"*

# Barclamp preparation (put them in the right places)
mkdir /opt/dell/barclamps
cd "$BASEDIR/dell/barclamps"
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
chmod +x  ${BASEDIR}/extra/*

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
    
if [[ $CHOSTNAME ]]; then
    
    cat > /install_system.sh <<EOF
#!/bin/bash
set -e
cd ${BASEDIR}/extra
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
