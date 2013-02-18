#!/bin/bash
set -e
die() {
    if [[ $crowbar_up && $FQDN ]]; then
        crowbar crowbar transition "$FQDN" problem
    fi
    echo "$(date '+%F %T %z'): $@"
    exit 1
}

knife() {
    command knife "$@" -u chef-webui -k /etc/chef/webui.pem || \
        die "knife $* failed"
}

# Figure out where we are installing from.
for p in redhat_dvd ubuntu_dvd; do
    [[ -d /tftpboot/$p ]] || continue
    DVD_PATH="/tftpboot/$p"
    BASEDIR="$DVD_PATH"
    break
done
unset p

if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
    OS=redhat
elif [[ -d /etc/apt ]]; then
    OS=ubuntu
elif [[ -f /etc/SuSE-release ]]; then
    OS=suse
else
    die "Staged on to unknown OS media!"
fi

# On SUSE based installs we don't (yet) rely on the DVD being copied
# the the harddisk. This might be subject to change.
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
    export PATH="/opt/dell/bin:/usr/local/bin:$PATH"
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

# Set up rsyslog to not rate limit to avoid discarding exceptions
cat > /etc/rsyslog.d/10-noratelimit.conf <<EOF
# Turn off rate limiting to prevent error discarding
\$ModLoad imuxsock
\$SystemLogRateLimitInterval 0
EOF

# Bounce rsyslog to let it know our hostname is correct and not to rate limit
if [[ $OS = suse ]]; then
    service syslog restart || :
else
    service rsyslog restart || :
fi

# Hack up sshd_config to kill delays
sed -i -e 's/^\(GSSAPI\)/#\1/' \
    -e 's/#\(UseDNS.*\)yes/\1no/' /etc/ssh/sshd_config
service sshd restart || :

if [[ $OS != suse ]]; then
    # Link the discovery image to an off-DVD location.
    # On SUSE the image is part of the crowbar-sledgehammer package
    [[ -d ${DVD_PATH}/discovery ]] && mv "${DVD_PATH}/discovery" "/tftpboot"
fi

if [[ $OS = ubuntu ]]; then
    if ! dpkg-query -S /opt/dell/bin/crowbar_crowbar; then
        (   cd "$DVD_PATH/extra"
            rabbit_chef_password=$( dd if=/dev/urandom count=1 bs=16 2>/dev/null | base64 | tr -d / )
            sed -i "s/__HOSTNAME__/$FQDN/g" ./debsel.conf
            sed -i "/^chef-solr/ s/password\$/${rabbit_chef_password}/" ./debsel.conf
            /usr/bin/debconf-set-selections ./debsel.conf)
        apt-get update
        apt-get -y install 'crowbar-barclamp-*'
    fi
elif [[ $OS = redhat ]]; then
    yum -y makecache
    yum -y install 'crowbar-barclamp-*'
elif [[ $OS = suse ]]; then
    zypper -n in -t pattern Crowbar_Admin
else
    die "Cannot install onto unknown OS $OS!"
fi

# all this can be skipped on SUSE based installs:
#  * all gems are available as packages
#  * the chef packages contain init scripts, no need to create
#    bluepill config and initscript here
if [[ $OS != suse ]]; then
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
            daemons bluepill xml-simple libxml-ruby wsman cstruct ; do
            gem install --local --no-ri --no-rdoc $gem-*.gem
        done
        cd ..
        gem generate_index)

    mkdir -p /var/run/bluepill
    mkdir -p /var/lib/bluepill
    mkdir -p /etc/bluepill

    # Copy all our pills to
    cp "$DVD_PATH/extra/"*.pill /etc/bluepill
    cp "$DVD_PATH/extra/chef-server.conf" /etc/nginx
    cp "$DVD_PATH/extra/chef-client.pill" /tftpboot

    [[ -L /opt/dell/extra ]] || ln -s "$DVD_PATH/extra" /opt/dell/extra

    if [[ ! -x /etc/init.d/bluepill ]]; then

        # Create an init script for bluepill
        cat > /etc/init.d/bluepill <<EOF
#!/bin/bash
# chkconfig: 2345 90 10
# description: Bluepill Daemon runner
PATH=$PATH
case \$1 in
    start) for pill in /etc/bluepill/*.pill; do
              [[ -f \$pill ]] || continue
              bluepill load "\$pill"
           done;;
    stop) bluepill stop
          bluepill quit;;
    status) if ps aux |grep [b]luepilld; then
             echo "Bluepill is running."
             exit 0
            else
             echo "Bluepill is not running."
             exit 1
            fi;;
    *) echo "\$1: Not supported.";;
esac
EOF

        # enable the bluepill init script and disable the old sysv init scripts.
        if which chkconfig &>/dev/null; then
            chkconfig bluepill on
        elif which update-rc.d &>/dev/null; then
            update-rc.d bluepill defaults 90 10
        else
            echo "Don't know how to handle services on this system!"
            exit 1
        fi
        chmod 755 /etc/init.d/bluepill
    fi
fi # [[ $OS != suse ]]

# Set up initial SSH keys if we don't have them
[[ -f $HOME/.ssh/id_rsa ]] || {
    mkdir -p "$HOME/.ssh"
    ssh-keygen -q -b 2048 -P '' -f "$HOME/.ssh/id_rsa"
    cat "$HOME/.ssh/id_rsa.pub" >> "$HOME/.ssh/authorized_keys"
}

# Run the rest of the barclamp install actions.
(cd /opt/dell/barclamps && /opt/dell/bin/barclamp_install.rb --deploy *)

for role in crowbar deployer-client "crowbar-${FQDN//./_}"; do
    knife node run_list add "$FQDN" role["$role"] || \
        die "Could not add $role to Chef. Crowbar bringup will fail."
done

# aaand... Go!
chef-client

# Make sure we have CROWBAR_KEY
export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
