#!/bin/bash
# Ubuntu specific chef install functionality
DVD_PATH="/tftpboot/ubuntu_dvd"
BASEDIR="/tftpboot/ubuntu_dvd"
[[ -f /etc/lsb-release ]] || {
    echo "No lsb-release file, cannot determine proper OS information!"
    exit 1
}
. /etc/lsb-release
OS_TOKEN="ubuntu-${DISTRIB_RELEASE}"

update_hostname() { update_hostname.sh $FQDN; }

install_base_packages() {
    cp apt.conf /etc/apt
    log_to apt sed -i "s/__HOSTNAME__/$FQDN/g" ./debsel.conf
    log_to apt /usr/bin/debconf-set-selections ./debsel.conf
    mkdir -p "/tftpboot/$OS_TOKEN/crowbar-extra"
    mkdir -p /etc/apt/sources.list.d
    (cd "/tftpboot/$OS_TOKEN/crowbar-extra";
        # Find all the staged barclamps
        for bc in "/opt/dell/barclamps/"*; do
            [[ -d $bc/cache/$OS_TOKEN/pkgs ]] || continue
            # Link them in.
            ln -s "$bc/cache/$OS_TOKEN/pkgs" "${bc##*/}"
            echo "deb file:/tftpboot/$OS_TOKEN/crowbar-extra/${bc##*/} /" > \
                /etc/apt/sources.list.d/10-barclamp-${bc##*/}.list
        done
    )
    log_to apt apt-get update
    log_to apt apt-get -y remove apparmor
    log_to apt apt-get -y install rubygems gcc ruby tcpdump \
        libcurl4-gnutls-dev build-essential ruby-dev libxml2-dev zlib1g-dev nginx \
        ipmitool efibootmgr

    # stop nginx
    service nginx stop
    rm -f /etc/nginx/sites-enabled/default
}

bring_up_chef() {
    log_to apt apt-get -y install chef kwalify
    service chef-client stop
    killall chef-client
    log_to apt apt-get -y install chef-server chef-server-webui
    (cd "$DVD_PATH/extra/patches"; chmod +x ./patch.sh ; ./patch.sh) || exit 1
    # increase chef-solr index field size
    perl -i -ne 'if ($_ =~ /<maxFieldLength>(.*)<\/maxFieldLength>/){ print "<maxFieldLength>200000</maxFieldLength> \n" } else { print } '  /var/lib/chef/solr/conf/solrconfig.xml

    # Fix ruby-gems and merb-core mismatch
    sed -i -e "s/Gem.activate(dep)/dep.to_spec.activate/g" /usr/lib/ruby/1.8/merb-core/core_ext/kernel.rb

    log_to svc service chef-server restart
}

pre_crowbar_fixups() { : ; }

post_crowbar_fixups() { : ; }

update_admin_node() {
    log_to apt apt-get -y upgrade
}

restart_ssh() {
    service ssh restart
}
