#!/bin/bash
# Redhat specific chef install functionality
DVD_PATH="/tftpboot/redhat_dvd"

update_hostname() {
    update_hostname.sh $FQDN
    source /etc/sysconfig/network
}

install_base_packages() {
    # Make sure we only try to install x86_64 packages.
    echo 'exclude = *.i386' >>/etc/yum.conf

    echo "$(date '+%F %T %z'): Installing "
    log_to yum yum -q -y update

    # Install the rpm and gem packages
    log_to yum yum -q -y install rubygems gcc make ruby-devel
}

bring_up_chef() {
    log_to yum yum -q -y install rubygem-chef-server rubygem-kwalify \
	curl-devel ruby-shadow

    # Default password in chef webui to password
    sed -i 's/web_ui_admin_default_password ".*"/web_ui_admin_default_password "password"/' /etc/chef/webui.rb

    ./start-chef-server.sh

    ## Missing client.rb for this system - Others get it ##
    touch /etc/chef/client.rb
    chown chef:chef /etc/chef/client.rb

    # HACK AROUND CHEF-2005
    di=$(find /usr/lib/ruby/gems/1.8/gems -name data_item.rb)
    cp -f patches/data_item.rb "$di"
    # HACK AROUND CHEF-2005
    rl=$(find /usr/lib/ruby/gems/1.8/gems -name run_list.rb)
    cp -f "$rl" "$rl.bak"
    cp -f patches/run_list.rb "$rl"
    ## END 2413 
    # HACK AROUND Kwalify and rake bug missing Gem.bin_path
    cp -f patches/kwalify /usr/bin/kwalify
    cp -f patches/rake /usr/bin/rake

    log_to svc service chef-server restart
}


# Make sure we use the right OS installer. By default we want to install
# the same OS as the admin node.
fix_up_os_deployer() {
    for t in provisioner deployer; do
	sed -i '/os_install/ s/os_install/redhat_install/' \
	    /opt/dell/barclamps/${t}/chef/data_bags/crowbar/bc-template-${t}.json
    done
}

pre_crowbar_fixups() {
    #patch bad gemspecs.
    cp $DVD_PATH/extra/patches/*.gemspec \
	/usr/lib/ruby/gems/1.8/specifications/
}

update_admin_node() {
    log_to yum yum -q -y upgrade
}

restart_ssh() {
    service sshd restart
}
