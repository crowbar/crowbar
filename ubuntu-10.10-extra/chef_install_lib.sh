#!/bin/bash
# Ubuntu specific chef install functionality
DVD_PATH="/tftpboot/ubuntu_dvd"
OS_TOKEN="ubuntu-10.10"
update_hostname() { update_hostname.sh $FQDN; }

install_base_packages() {
    cp apt.conf /etc/apt
    log_to apt sed -i "s/__HOSTNAME__/$FQDN/g" ./debsel.conf
    log_to apt /usr/bin/debconf-set-selections ./debsel.conf
     # First, make a repo for crowbar-extras
    apt-get -y install dpkg-dev
    mkdir -p "/tftpboot/$OS_TOKEN/crowbar-extra"
    (cd "/tftpboot/$OS_TOKEN/crowbar-extra";
	# Find all the staged barclamps
	for bc in "/opt/dell/barclamps/"*; do
	    [[ -d $bc/cache/$OS_TOKEN/pkgs ]] || continue
	    # Link them in.
	    ln -s "$bc/cache/$OS_TOKEN/pkgs" "${bc##*/}"
	done
	dpkg-scanpackages . 2>/dev/null |gzip -9 >Packages.gz)
    echo "deb file:/tftpboot/$OS_TOKEN/crowbar-extra /" >>/etc/apt/sources.list
    log_to apt apt-get update
    log_to apt apt-get -y remove apparmor
    log_to apt apt-get -y install rubygems gcc ruby \
	libcurl4-gnutls-dev build-essential ruby-dev 
}

bring_up_chef() {
    log_to apt apt-get -y install chef kwalify
    service chef-client stop
    killall chef-client
    log_to apt apt-get -y install chef-server chef-server-webui

    # HACK AROUND CHEF-2005
    cp patches/data_item.rb /usr/share/chef-server-api/app/controllers
    # HACK AROUND CHEF-2005
    rl=$(find /usr/lib/ruby -name run_list.rb)
    cp -f "$rl" "$rl.bak"
    cp -f patches/run_list.rb "$rl"
    # Make the Rubygems provider in Chef respect gemrc files.
    cp -f patches/rubygems.rb /usr/lib/ruby/vendor_ruby/chef/provider/package
    log_to svc service chef-server restart
}

# Make sure we use the right OS installer. By default we want to install
# the same OS as the admin node.
fix_up_os_deployer() {
    for t in provisioner deployer; do
	sed -i '/os_install/ s/os_install/ubuntu_install/' \
	    /opt/dell/barclamps/${t}/chef/data_bags/crowbar/bc-template-${t}.json
    done
}

pre_crowbar_fixups() { : ; }

update_admin_node() {
    log_to apt apt-get -y upgrade
}

restart_ssh() {
    service ssh restart
}
