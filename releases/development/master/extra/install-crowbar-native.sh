#!/bin/bash
set -e
die() {
    echo "$(date '+%F %T %z'): $@"
    exit 1
}

# Figure out where we are installing from.
for p in redhat_dvd ubuntu_dvd; do
    [[ -d /tftpboot/$p ]] || continue
    DVD_PATH="/tftpboot/$p"
    BASEDIR="$DVD_PATH"
    break
done
[[ -d /srv/tftpboot/opensuse_dvd ]] && DVD_PATH="/srv/tftpboot/opensuse_dvd"
unset p

touch /tmp/.crowbar_in_bootstrap
if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
    OS=redhat
    yum -y install ruby rubygems ruby-devel libxml2-devel zlib-devel gcc make
elif [[ -f /etc/SuSE-release ]]; then
    OS=suse
    ( grep openSUSE /etc/SuSE-release ) && OS=opensuse
    zypper ll | cut -f1 -d ' ' | xargs zypper --non-interactive rl
    zypper install -y -l ruby ruby19 ruby-devel ruby19-devel libxml2-devel \
        libxslt1 libxslt-devel libxslt-tools zlib-devel rsyslog \
        postgresql93 postgresql93-server postgresql93-contrib libpq5 \
        libossp-uuid16 libecpg6
    # Hack up local postgres to only listen on domain sockets.
    # Need to start postresql to create database control directories
    #  then stop it so we can edit the configuration.
    rcpostgresql start
    rcpostgresql stop
    cat >/var/lib/pgsql/data/pg_hba.conf <<EOF
local   all             postgres                                peer
local   all             all                                     trust
EOF
    echo "listen_addresses = ''" >>/var/lib/pgsql/data/postgresql.conf
    sed -i '/^port/ s/5432/5439/' /var/lib/pgsql/data/postgresql.conf
    sed -i 's/#port/port/' /var/lib/pgsql/data/postgresql.conf
    rcpostgresql restart
    sudo -H -u postgres createuser -p 5439 -d -S -R -w crowbar
elif [[ -d /etc/apt ]]; then
    OS=ubuntu
    apt-get -y install ruby1.9.1 ruby1.9.1-dev \
        libxml2-dev libxslt1-dev zlib1g-dev \
        postgresql-9.3 postgresql-client-9.3 postgresql-contrib-9.3 libpq-dev
    # Hack up local postgres to only listen on domain sockets.
    service postgresql stop
    cat >/etc/postgresql/9.3/main/pg_hba.conf <<EOF
local   all             postgres                                peer
local   all             all                                     trust
EOF
    echo "listen_addresses = ''" >>/etc/postgresql/9.3/main/postgresql.conf
    sed -i '/^port/ s/5432/5439/' /etc/postgresql/9.3/main/postgresql.conf
    service postgresql start
    sudo -H -u postgres createuser -p 5439 -d -S -R -w crowbar
else
    die "Staged on to unknown OS media!"
fi

# On SUSE SLE based installs we don't (yet) rely on the DVD being copied
# the the harddisk. This might be subject to change. On openSUSE we expect
# the DVD to have been copied to the tftpboot directory.
if [[ $OS != suse ]]; then
    [[ $DVD_PATH ]] || die "Cannot find our install source!"
fi

if [[ -f /opt/dell/crowbar_framework/.crowbar-installed-ok ]]; then
    echo "Crowbar is already installed, refusing to let install run."
    echo "If you really want to do this, "
    echo "remove /opt/dell/crowbar_framework/.crowbar-installed-ok"
    exit 1
fi

export FQDN="$1"
export DEBUG=true

if [[ $OS != suse ]]; then
    [[ $PATH != */opt/dell/bin* ]] || export PATH="$PATH:/opt/dell/bin"
    if [[ -f /etc/environment ]] && ! grep -q '/opt/dell/bin' /etc/environment; then
        sed -i -e "/^PATH/ s@\"\(.*\)\"@\"$PATH\"@" /etc/environment
    fi
    [[ ! $HOME || $HOME = / ]] && export HOME="/root"
    mkdir -p "$HOME"

    # Set Version for all to use.
    VERSION=$(cat $DVD_PATH/dell/Version)
    echo "Installing admin with version: $VERSION"

    mkdir -p /opt/dell/bin
    cp -a "$DVD_PATH/extra/"*.rb /opt/dell/bin

    # Verify that our install bits are intact.
    if [[ ! -f $DVD_PATH/sha1_passed ]]; then
        (cd $DVD_PATH && sha1sum -c sha1sums &>/dev/null) || \
        die "SHA1sums do not match, install is corrupt."
        >$DVD_PATH/sha1_passed
    fi
fi

fqdn_re='^[0-9a-zA-Z.-]+$'
# Make sure there is something of a domain name
export DOMAINNAME=${FQDN#*.}
export HOSTNAME=$FQDN
[[ $DOMAINNAME = $FQDN || $DOMAINNAME = ${DOMAINNAME#*.} ]] && \
    die "Please specify an FQDN for the admin name"
[[ $FQDN =~ $fqdn_re ]] || \
    die "Please specify an FQDN for the admin name with valid characters"

# On SUSE the assumption is that the hostname is already set during
# base os installation
if [[ $OS != suse ]]; then
    echo "$(date '+%F %T %z'): Setting Hostname..."
    /opt/dell/bin/update_hostname.sh "$FQDN" || die "Could not update our hostname"
fi

# Bounce rsyslog to let it know our hostname is correct and not to rate limit
if [[ $OS = suse ]]; then
    service syslog restart || :
else
    # Set up rsyslog to not rate limit to avoid discarding exceptions
    cat > /etc/rsyslog.d/10-noratelimit.conf <<EOF
# Turn off rate limiting to prevent error discarding
\$ModLoad imuxsock
\$SystemLogRateLimitInterval 0
EOF

    service rsyslog restart || :
fi

# Set up initial SSH keys if we don't have them
[[ -f $HOME/.ssh/id_rsa ]] || {
    mkdir -p "$HOME/.ssh"
    ssh-keygen -q -b 2048 -P '' -f "$HOME/.ssh/id_rsa"
    cat "$HOME/.ssh/id_rsa.pub" >> "$HOME/.ssh/authorized_keys"
}

# Hack up sshd_config to kill delays
sed -i -e 's/^\(GSSAPI\)/#\1/' \
    -e 's/#\(UseDNS.*\)yes/\1no/' /etc/ssh/sshd_config
service sshd restart || :

if [[ $OS == opensuse ]]; then
   # Link the discovery image to an off-DVD location.
    # On SUSE the image is part of the crowbar-sledgehammer package
    [[ -d ${DVD_PATH}/discovery ]] && mv "${DVD_PATH}/discovery" "/srv/tftpboot"
    
    # Lift the gems off the install media for easy file serving.
    mkdir -p /srv/tftpboot/gemsite/gems
    find "/opt/dell/barclamps" -path '*/gems/*.gem' \
        -exec ln -sf '{}' /srv/tftpboot/gemsite/gems ';'
    
    # Arrange for all our gem binaries to be installed into /usr/local/bin
    cat >/etc/gemrc <<EOF
:sources:
- file:///srv/tftpboot/gemsite/
gem: --no-ri --no-rdoc --bindir /usr/local/bin
EOF
elif [[ $OS != suse ]]; then
    # Link the discovery image to an off-DVD location.
    # On SUSE the image is part of the crowbar-sledgehammer package
    [[ -d ${DVD_PATH}/discovery ]] && mv "${DVD_PATH}/discovery" "/tftpboot"

    # Lift the gems off the install media for easy file serving.
    mkdir -p /tftpboot/gemsite/gems
    find "/opt/dell/barclamps" -path '*/gems/*.gem' \
        -exec ln -sf '{}' /tftpboot/gemsite/gems ';'

    # Arrange for all our gem binaries to be installed into /usr/local/bin
    cat >/etc/gemrc <<EOF
:sources:
- file:///tftpboot/gemsite/
gem: --no-ri --no-rdoc --bindir /usr/local/bin
EOF
    # This is ugly, but there does not seem to be a better way
    # to tell Chef to just look in a specific location for its gems.
    echo "$(date '+%F %T %z'): Arranging for gems to be installed"
    (   cd /tftpboot/gemsite/gems
        for gem in builder json net-http-digest_auth activesupport i18n \
            daemons xml-simple libxml-ruby wsman cstruct ; do
            gem install --local --no-ri --no-rdoc $gem-*.gem || :
        done
        cd ..
        gem generate_index
    )
fi

if [[ $OS = opensuse ]]; then
    # Link the discovery image to an off-DVD location.
    [[ -d ${DVD_PATH}/discovery ]] && mv "${DVD_PATH}/discovery" "/srv/tftpboot"

    # Lift the gems off the install media for easy file serving.
    mkdir -p /srv/tftpboot/gemsite/gems
    find "/opt/dell/barclamps" -path '*/gems/*.gem' \
        -exec ln -sf '{}' /srv/tftpboot/gemsite/gems ';'

    # Arrange for all our gem binaries to be installed into /usr/local/bin
    cat >/etc/gemrc <<EOF
:sources:
- file:///srv/tftpboot/gemsite/
gem: --no-ri --no-rdoc --bindir /usr/local/bin
EOF

    # This is ugly, but there does not seem to be a better way
    # to tell Chef to just look in a specific location for its gems.
    echo "$(date '+%F %T %z'): Arranging for gems to be installed"
    (   cd /srv/tftpboot/gemsite/gems
        for gem in builder json net-http-digest_auth activesupport i18n \
            daemons xml-simple libxml-ruby wsman cstruct ; do
            gem install --local --no-ri --no-rdoc $gem-*.gem || :
        done
        cd ..
        gem generate_index
    )
fi


if [[ $OS = ubuntu ]]; then
    if ! dpkg-query -S /opt/dell/bin/crowbar_crowbar; then
        (   cd "$DVD_PATH/extra"
            sed -i "s/__HOSTNAME__/$FQDN/g" ./debsel.conf
            /usr/bin/debconf-set-selections ./debsel.conf)
        apt-get update
        apt-get -y install libssl-dev 'crowbar-barclamp-*'
    fi
elif [[ $OS = redhat ]]; then
    yum -y makecache
    yum -y install 'crowbar-barclamp-*'
elif [[ $OS = suse ]]; then
    zypper --gpg-auto-import-keys -n install -t pattern Crowbar_Admin
elif [[ $OS = opensuse ]]; then
    zypper install crowbar-barclamp-\*
    sed -ie 's/tftpboot/srv\/tftpboot/' /opt/dell/crowbar_framework/Gemfile
else
    die "Cannot install onto unknown OS $OS!"
fi

###
# All this should migrate to being part of the crowbar metapackage!
###

# Install prerequisite gems
if [[ $OS = suse ]]; then
    BUNDLE_INSTALL_ARGS="--local"
else
    gem install bundler rake
    BUNDLE_INSTALL_ARGS="--path vendor/bundle"
fi

(cd /opt/dell/crowbar_framework; bundle install $BUNDLE_INSTALL_ARGS)

if ! grep -q crowbar /etc/sudoers; then
    chmod u+w /etc/sudoers
    echo 'crowbar ALL=(ALL:ALL) NOPASSWD: ALL' >>/etc/sudoers
    chmod u-w /etc/sudoers
fi

mkdir -p /var/run/crowbar/
chmod 0700 /var/run/crowbar

# Fix up /etc/environment
if ! grep -q '/opt/dell/bin' /etc/environment; then
    export PATH="$PATH:/opt/dell/bin"
    sed -i -e "/^PATH/ s@\"\(.*\)\"@\"$PATH\"@" /etc/environment
fi

# make sure RAILS_ENV is set to production
if ! grep -q 'RAILS_ENV' /etc/environment; then
    echo "export RAILS_ENV=production" >> /etc/environment
fi

# Make our /etc/profile/crowbar.sh
if [[ ! -f /etc/profile.d/crowbar.sh ]]; then
    mkdir -p /etc/profile.d
    cat > /etc/profile.d/crowbar.sh <<EOF
# Make sure that CROWBAR_KEY is in the environment
if [ -f /etc/crowbar.install.key ] ; then
    export CROWBAR_KEY=\$(cat /etc/crowbar.install.key)
fi
EOF
fi
cd /opt/dell/crowbar_framework

for d in /var/run/crowbar /opt/dell/crowbar_framework; do
    chown -R crowbar:crowbar "$d"
done

# Put framework docs in place
mkdir -p /opt/dell/doc
[[ -d $DVD_PATH/doc/framework ]] && cp -a $DVD_PATH/doc/framework /opt/dell/doc

# Run the rest of the barclamp install actions.
(export DEBUG=true; cd /opt/dell/barclamps && /opt/dell/bin/barclamp_install.rb --deploy *)

# Get out of bootstrap mode
rm -f /tmp/.crowbar_in_bootstrap
service crowbar restart

# By now, we have a machine key.  Load it.
export CROWBAR_KEY=$(cat /etc/crowbar.install.key)

# Eventaully, --wizard will become the default.
if ! [[ $* = *--wizard* ]]; then
    ###
    # This should vanish once we have a real bootstrapping story.
    ###
    ip_re='([0-9a-f.:]+/[0-9]+)'

    # Create a stupid default admin network
    curl -s -f --digest -u $(cat /etc/crowbar.install.key) \
        -X POST http://localhost:3000/network/api/v2/networks \
        -d "name=admin" \
        -d "deployment=system" \
        -d "conduit=1g0"  \
        -d 'ranges=[ { "name": "admin", "first": "192.168.124.10/24", "last": "192.168.124.11/24"},{"name": "host", "first": "192.168.124.81/24", "last": "192.168.124.254/24"},{"name": "dhcp", "first": "192.168.124.21/24", "last": "192.168.124.80/24"}]'

    # Create the admin node entry.
    curl -s -f --digest -u $(cat /etc/crowbar.install.key) \
        -X POST http://localhost:3000/api/v2/nodes \
        -d "name=$FQDN" \
        -d 'admin=true' \
        -d 'alive=false' \
        -d 'bootenv=local'

    # Figure out what IP addresses we should have, and add them.
    netline=$(curl -f --digest -u $(cat /etc/crowbar.install.key) -X GET "http://localhost:3000/network/api/v2/networks/admin/allocations" -d "node=$(hostname -f)")
    nets=(${netline//,/ })
    for net in "${nets[@]}"; do
        [[ $net =~ $ip_re ]] || continue
        net=${BASH_REMATCH[1]}
        # Make this more complicated and exact later.
        ip addr add "$net" dev eth0 || :
        echo "${net%/*} $FQDN" >> /etc/hosts
    done

    # Mark the node as alive.
    curl -s -f --digest -u $(cat /etc/crowbar.install.key) \
        -X PUT "http://localhost:3000/api/v2/nodes/$FQDN" \
        -d 'alive=true'
    
    # Converge the admin node.
    tries=3
    converged=false
    while ((tries > 0)); do
        echo "Converging all noderoles on $FQDN ($tries tries left):"
        if /opt/dell/bin/crowbar converge; then
            converged=true
            break
        fi
        tries=$((tries - 1))
    done
    [[ $converged = false ]] && die "Could not converge $FQDN!"
fi

echo "Admin node deployed."

touch /opt/dell/crowbar_framework/.crowbar-installed-ok

