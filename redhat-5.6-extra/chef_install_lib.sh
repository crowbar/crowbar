#!/bin/bash
# Redhat specific chef install functionality
DVD_PATH="/tftpboot/redhat_dvd"
OS_TOKEN="redhat-5.6"
update_hostname() {
    update_hostname.sh $FQDN
    source /etc/sysconfig/network
}

install_base_packages() {
    # Make sure we only try to install x86_64 packages.
    
    yum -q -y install createrepo
    # Make our local cache
    mkdir -p "/tftpboot/$OS_TOKEN/crowbar-extra"
    (cd "/tftpboot/$OS_TOKEN/crowbar-extra";
	# Find all the staged barclamps
	for bc in "/opt/dell/barclamps/"*; do
	    [[ -d $bc/cache/$OS_TOKEN/pkgs ]] || continue
	    # Link them in.
	    ln -s "$bc/cache/$OS_TOKEN/pkgs" "${bc##*/}"
	done
	createrepo -d -q .)
    cat >/etc/yum.repos.d/crowbar-xtras.repo <<EOF
[crowbar-xtras]
name=Crowbar Extra Packages
baseurl=file:///tftpboot/$OS_TOKEN/crowbar-extras
gpgcheck=0
EOF

    echo 'exclude = *.i386' >>/etc/yum.conf

    echo "$(date '+%F %T %z'): Installing "
    log_to yum yum -q -y update

    # Install the rpm and gem packages
    log_to yum yum -q -y install rubygems gcc make ruby-devel
}

bring_up_chef() {
    log_to yum yum -q -y install rubygem-chef rubygem-kwalify
    service chef-client stop
    killall chef-client
    log_to yum yum -q -y install rubygem-chef-server \
	curl-devel ruby-shadow patch

    # Default password in chef webui to password
    sed -i 's/web_ui_admin_default_password ".*"/web_ui_admin_default_password "password"/' /etc/chef/webui.rb

    # HACK AROUND OHAI redhatenterpriselinux
    di=$(find /usr -path '*/ohai-0.6.6/lib/ohai/plugins/linux' -type d)
    [[ -d $di ]] && {
	cp patches/ohai-linux-platform.patch "$di"
	(cd "$di"; patch -p0 <ohai-linux-platform.patch)
    }
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
