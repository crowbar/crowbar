#!/bin/bash
# OpenSUSE specific chef install functionality
DVD_PATH="/tftpboot/opensuse_dvd"
update_hostname() {
    update_hostname.sh $FQDN
    source /etc/sysconfig/network
}

install_base_packages() {
    > /etc/zypp/repos.d/crowbar-xtras.repo
    # Make our local cache
    mkdir -p "/tftpboot/$OS_TOKEN/crowbar-extra"
    (cd "/tftpboot/$OS_TOKEN/crowbar-extra";
        # Find all the staged barclamps
        for bc in "/opt/dell/barclamps/"*; do
            [[ -d $bc/cache/$OS_TOKEN/pkgs/repodata ]] || continue
            # Link them in.
            ln -s "$bc/cache/$OS_TOKEN/pkgs" "${bc##*/}"
        done
    )

    log_to zypper zypper update -l -y

    # Install the rpm and gem packages
    log_to zypper zypper install -l -y rubygem\* gcc make ruby-devel \
        libxml2-devel zlib-devel tcpdump nginx efibootmgr ruby-zyp\*

    # stop nginx
    service nginx stop
    rm -f /etc/nginx/sites-enabled/default
}

bring_up_chef() {
    log_to zypper zypper install -l -y rubygem-chef rubygem-kwalify
    service chef-client stop
    killall chef-client
    log_to zypper zypper install -l -y rubygem-chef-server \
        curl-devel ruby-shadow patch
    (cd "$DVD_PATH/extra/patches"; chmod +x ./patch.sh; ./patch.sh) || exit 1
    # Default password in chef webui to password
    sed -i 's/web_ui_admin_default_password ".*"/web_ui_admin_default_password "password"/' /etc/chef/webui.rb
    ./start-chef-server.sh

    ## Missing client.rb for this system - Others get it ##
    touch /etc/chef/client.rb
    chown chef:chef /etc/chef/client.rb

   # increase chef-solr index field size
    perl -i -ne 'if ($_ =~ /<maxFieldLength>(.*)<\/maxFieldLength>/){ print "<maxFieldLength>200000</maxFieldLength> \n" } else { print } '  /var/chef/solr/conf/solrconfig.xml
    log_to svc service chef-server restart
}

pre_crowbar_fixups() {
    #patch bad gemspecs.
    cp $DVD_PATH/extra/patches/*.gemspec \
        /usr/lib/ruby/gems/1.9.1/specifications/
}

post_crowbar_fixups() { : ; }

update_admin_node() {
    log_to zypper zypper upgrade -y -l
}

restart_ssh() {
    service sshd restart
}
