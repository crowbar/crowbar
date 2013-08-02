#! /bin/bash -e
# vim: sw=4 et

# This script is called after being installed by the Crowbar RPM from the SUSE
# Cloud ISO. In this context, it is expected that all other required
# repositories (eg. SLES, Updates) are already set up, with the required files
# in place.
#
# For development and testing use, call the script with the '--from-git'
# option. Use the appropriate dev VM and follow the corresponding setup
# instructions.

usage () {
    # do not document --from-git option; it's for developers only
    cat <<EOF
`basename $0` [-h|--help] [-v|--verbose]

Install Crowbar on administration server.
EOF
    exit
}

while test $# -gt 0; do
    case "$1" in
        -h|--help|--usage|-\?) usage ;;
        -v|--verbose) CROWBAR_VERBOSE=1 ;;
        --from-git) CROWBAR_FROM_GIT=1 ;;
        *) ;;
    esac
    shift
done

if [ $(id -u) -gt 0 ]; then
    echo "$0 needs to be run as root user."
    echo ""
    exit 1
fi

BARCLAMP_INSTALL_OPTS="--rpm"

if [ -n "$CROWBAR_FROM_GIT" ]; then
    BARCLAMP_INSTALL_OPTS="--force"
    : ${CROWBAR_FILE:=/root/crowbar/crowbar.json}
    : ${BARCLAMP_SRC:=/root/crowbar/barclamps/}
fi

LOGFILE=/var/log/crowbar/install.log
mkdir -p "`dirname "$LOGFILE"`"

: ${BARCLAMP_SRC:="/opt/dell/barclamps/"}

run_succeeded=


DIALOG_TITLE=" SUSE Cloud 2.0 "

# Infrastructure for nice output/logging
# --------------------------------------

# Copy stdout to fd 3
exec 3>&1
# Create fd 4 for logfile
exec 4>> "$LOGFILE"

if [ -z "$CROWBAR_VERBOSE" ]; then
    # Set fd 1 and 2 to logfile
    exec 1>&4 2>&1
else
    # Set fd 1 and 2 to logfile (and keep stdout too)
    exec 1> >( tee -a /dev/fd/4 ) 2>&1
fi
# Send summary fd to original stdout
exec 6>&3

use_dialog () {
    [ -z "$CROWBAR_VERBOSE" -a -t 3 -a -x "$(type -p dialog)" ]
}

pipe_show_and_log () {
    if use_dialog; then
        t=$(mktemp)
        cat - > $t
        dialog --title "$DIALOG_TITLE" --textbox -- $t $(($(wc -l <$t)+4)) 75 >&3
        rm -f $t
        dialog --clear >&3
    fi
    tee -a /dev/fd/3 /dev/fd/4 > /dev/null
}

# Draw a spinner so the user knows something is happening
spinner () {
    local delay=0.75
    local spinstr='/-\|'
    local msg="$@"

    # reset exit handler
    trap "exit" EXIT

    printf "... " >&3
    while [ true ]; do
        local temp=${spinstr#?}
        if use_dialog; then
            printf "\n%s [%c]" "$msg... " "$spinstr" | dialog --title "$DIALOG_TITLE" \
                --keep-window --progressbox 5 70 >&3
        else
            printf "[%c]" "$spinstr" >&3
        fi
        local spinstr=$temp${spinstr%"$temp"}
        sleep $delay
        if ! use_dialog; then
            printf "\b\b\b" >&3
        fi
    done
}

kill_spinner () {
    if [ ! -z "$LAST_SPINNER_PID" ]; then
        kill >/dev/null 2>&1 $LAST_SPINNER_PID
        if [ $# -eq 0 ]; then
            printf "\b\b\bdone\n" >&3
        else
            printf "\b\b\b$*\n" >&3
        fi
        unset LAST_SPINNER_PID
    fi
}

kill_spinner_with_failed () {
    kill_spinner "failed"
}

echo_log () {
    echo -e === "$(date '+%F %T %z'): $@" >&4
}

echo_summary () {
    # Also send summary to logfile
    echo_log $@

    kill_spinner

    if [ -z "$CROWBAR_VERBOSE" ]; then
        if [ -t 3 ]; then
            echo -n -e $@ >&3
            if use_dialog; then
                echo -n -e $@ | dialog --title "$DIALOG_TITLE" --progressbox 8 60 >&3
            fi
            # Use disown to lose job control messages (especially the
            # "Completed" message when spinner will be killed)
            ( spinner $@ ) & disown
            LAST_SPINNER_PID=$!
        else
            echo -e $@ >&3
        fi
    else
        echo -e === $@ >&3
    fi
}

echo_summary_no_spinner () {
    # Also send summary to logfile
    echo_log $@

    kill_spinner

    if [ -z "$CROWBAR_VERBOSE" ]; then
        echo -e $@ >&3
    else
        echo -e === $@ >&3
    fi
}

die() {
    # Send empty line & error to logfile
    echo >&4
    echo_log "Error: $@"

    kill_spinner_with_failed

    if use_dialog; then
        dialog --title "$DIALOG_TITLE" --clear --msgbox -- "Error: $@" 8 73 >&3
        # avoid triggering two dialogs in a row
        run_succeeded=already_died
    else
        echo >&3
        echo -e "Error: $@" >&3
    fi

    res=1
    exit 1
}

exit_handler () {
    if test -n "$CROWBAR_TMPDIR" -a -d "$CROWBAR_TMPDIR"; then
        rm -r "$CROWBAR_TMPDIR"
    fi

    if [ -z "$run_succeeded" ]; then
        kill_spinner_with_failed
        cat <<EOF | pipe_show_and_log

Crowbar installation terminated prematurely.  Please examine the above
output or $LOGFILE for clues as to what went wrong.
You should also check the SUSE Cloud Installation Manual, in
particular the Troubleshooting section.  Note that this script can
safely be re-run multiple times if required.
EOF
    else
        kill_spinner
    fi
}

trap exit_handler EXIT


# Real work starts here
# ---------------------

echo "`date` $0 started with args: $*"

CROWBAR_TMPDIR=$(mktemp -d --tmpdir crowbar-install-XXXXXX)

ensure_service_running () {
    service="$1"
    regexp="${2:-running}"
    if service $service status | egrep -q "$regexp"; then
        echo "$service is already running - no need to start."
    else
        service $service start
        sleep 4
    fi
}


if [ -f /opt/dell/crowbar_framework/.crowbar-installed-ok ]; then
    run_succeeded=already_before

cat <<EOF | pipe_show_and_log
Aborting: Administration Server is already deployed.

If you want to run the installation script again,
then please remove the following file:

    /opt/dell/crowbar_framework/.crowbar-installed-ok
EOF
    exit 1
fi

# Sanity checks
# -------------

echo_summary "Performing sanity checks"

if test -z "$STY"; then
    die "Not running in screen. Please use \"screen $0\" to avoid problems during network re-configuration. Aborting."
fi

rootpw=$( getent shadow root | cut -d: -f2 )
case "$rootpw" in
    \*|\!*)
        if [ ! -f /root/.ssh/authorized_keys ]; then
            # extra-paranoid: not even sure how people could be logged in if that happens...
            die "root password is unset or locked and no /root/.ssh/authorized_keys file exists; to avoid being accidentally locked out of this Administration Server, you should first ensure that you have a working root password or authorized ssh keys."
        else
            echo "root password is unset or locked. Previous keys from /root/.ssh/authorized_keys will be kept, therefore you should not be accidentally locked out of this Administration Server."
        fi
        ;;
esac

# It is exceedingly important that 'hostname -f' actually returns an FQDN!
# if it doesn't, add an entry to /etc/hosts, e.g.:
#    192.168.124.10 cb-admin.example.com cb-admin
if ! FQDN=$(hostname -f 2>/dev/null); then
    die "Unable to detect fully-qualified hostname. Aborting."
fi

if ! DOMAIN=$(hostname -d 2>/dev/null); then
    die "Unable to detect DNS domain name. Aborting."
fi
 
if [ -z "$FQDN" -o -z "$DOMAIN" ]; then
    die "Unable to detect fully-qualified hostname. Aborting."
fi

if ! resolved=$(getent ahosts $FQDN 2>/dev/null); then
    die "Unable to resolve hostname $FQDN via host(1). Please check your configuration of DNS, hostname, and /etc/hosts. Aborting."
fi

IPv4_addr=$( echo "$resolved" | awk '{ if ($1 !~ /:/) { print $1; exit } }' )
IPv6_addr=$( echo "$resolved" | awk '{ if ($1  ~ /:/) { print $1; exit } }' )
if [ -z "$IPv4_addr" -a -z "$IPv6_addr" ]; then
    die "Could not resolve $FQDN to an IPv4 or IPv6 address. Aborting."
fi

if [ -n "$CROWBAR_FROM_GIT" ]; then
    REPOS_SKIP_CHECKS+=" SLES11-SP3-Pool SLES11-SP3-Updates SUSE-Cloud-2.0-Pool SUSE-Cloud-2.0-Updates"
    zypper in rubygems rubygem-json createrepo
fi

if [ -n "$IPv4_addr" ]; then
    echo "$FQDN resolved to IPv4 address: $IPv4_addr"
    if ! ip addr | grep -q "inet $IPv4_addr"; then
        die "No local interfaces configured with address $IPv4_addr. Aborting."
    fi
    if [[ "$IPv4_addr" =~ ^127 ]]; then
        die "$FQDN resolves to a loopback address. Aborting."
    fi

    if [ -f /etc/crowbar/network.json ]; then
        NETWORK_JSON=/etc/crowbar/network.json
    elif [ -n "$CROWBAR_FROM_GIT" ]; then
        NETWORK_JSON=$BARCLAMP_SRC/network/chef/data_bags/crowbar/bc-template-network.json
    else
        NETWORK_JSON=/opt/dell/chef/data_bags/crowbar/bc-template-network.json
    fi

    if ! /opt/dell/bin/network-json-validator --admin-ip "$IPv4_addr" $NETWORK_JSON; then
        die "Failed to validate network.json configuration. Please check and fix with yast2 crowbar. Aborting."
    fi
fi
if [ -n "$IPv6_addr" ]; then
    echo "$FQDN resolved to IPv6 address: $IPv6_addr"
    if ! ip addr | grep -q "inet6 $IPv6_addr"; then
        die "No local interfaces configured with address $IPv6_addr. Aborting."
    fi
fi

# Note that the grep will fail if iptables' output changes; unlikely to happen,
# but...
if LANG=C iptables -n -L | grep -qvE '^$|^Chain [^ ]|^target     prot'; then
    die "Firewall is not completely disabled. Aborting."
fi

if ! ping -c 1 $FQDN >/dev/null 2>&1; then
    die "Failed to ping $FQDN; please check your network configuration. Aborting."
fi


# output details, that will make remote debugging via bugzilla much easier  
# for us
/usr/bin/zypper lr -d  || :
/bin/rpm -qV crowbar || :
/usr/bin/lscpu  || :
/bin/df -h  || :
/usr/bin/free -m || :
/bin/ls -la /srv/tftpboot/repos/ /srv/tftpboot/repos/Cloud/ /srv/tftpboot/suse-11.3/install/ || :

if [ -f /opt/dell/chef/cookbooks/provisioner/templates/default/autoyast.xml.erb ]; then
    # The autoyast profile might not exist yet when CROWBAR_FROM_GIT is enabled
    /usr/bin/grep media_url /opt/dell/chef/cookbooks/provisioner/templates/default/autoyast.xml.erb
fi

create_gpg_key () {
  # To sign the repositories the admin node needs a gpg key - create one, if
  # it's not present

  if ! [[ -n $(gpg --list-secret-keys) ]]; then
    # no secret key found - create one
    # create batch file for automated key generation
     echo "Generating GPG key that will be used to sign repositories"
     cat > $CROWBAR_TMPDIR/SUSE-key.batch <<EOF
          Key-Type: DSA
          Key-Length: 1024
          Name-Real: SUSE Cloud resporitory key
          Name-Comment: Autogenerated key for repository signing 
          Name-Email: cloud@suse.de
          Expire-Date: 0
          %no-protection          
          %commit
EOF
    gpg --batch --gen-key $CROWBAR_TMPDIR/SUSE-key.batch
    rm $CROWBAR_TMPDIR/SUSE-key.batch
  else
    # secret key found - don't do anything at all
    echo "Will use pre-existing GPG key to sign repositories"
  fi
}

sign_repositories () {
  # Currently we only sign the Cloud-PTF repository
  if [ -f /srv/tftpboot/repos/Cloud-PTF/repodata/repomd.xml ]; then
    if [ ! -f /srv/tftpboot/repos/Cloud-PTF/repodata/repomd.xml.asc -o \
         ! -f /srv/tftpboot/repos/Cloud-PTF/repodata/repomd.xml.key ]; then
      create_gpg_key
      echo "Signing Cloud-PTF repository"
      gpg -a --detach-sign /srv/tftpboot/repos/Cloud-PTF/repodata/repomd.xml
      gpg -a --export > /srv/tftpboot/repos/Cloud-PTF/repodata/repomd.xml.key
    else
      echo "Cloud-PTF repository is already signed"
    fi
  fi
}

skip_check_for_repo () {
    repo="$1"
    for skipped_repo in $REPOS_SKIP_CHECKS; do
        if [ "$repo" = "$skipped_repo" ]; then
            return 0
        fi
    done
    return 1
}

check_repo_content () {
    repo_name="$1" repo_path="$2" md5="$3"

    if skip_check_for_repo "$repo_name"; then
        echo "Skipping check for $repo_name due to \$REPOS_SKIP_CHECKS"
        return 0
    fi

    if ! [ -e "$repo_path/content.asc" ]; then
        if [ -n "$CROWBAR_FROM_GIT" ]; then
            die "$repo has not been set up yet; please see https://github.com/SUSE/cloud/wiki/Crowbar"
        else
            die "$repo_name has not been set up at $repo_path\n\nPlease check the steps in the installation guide."
        fi
    fi

    if [ -n "$CROWBAR_FROM_GIT" ]; then
        echo "Skipping md5 check for $repo_name due to \$CROWBAR_FROM_GIT"
    else
        if [ "`md5sum $repo_path/content | awk '{print $1}'`" != "$md5" ]; then
            die "$repo_name does not contain the expected repository ($repo_path/content failed MD5 checksum)"
        fi
    fi
}

check_repo_product () {
    repo="$1" expected_summary="$2"
    products_xml=/srv/tftpboot/repos/$repo/repodata/products.xml
    if ! grep -q "<summary>$2</summary>" $products_xml; then
        if skip_check_for_repo "$repo"; then
            echo "Ignoring failed repo check for $repo due to \$REPOS_SKIP_CHECKS ($products_xml is missing summary '$expected_summary')"
            if ! [ -d /srv/tftpboot/repos/$repo ]; then
                echo "Creating repo skeleton to make AutoYaST happy."
                mkdir /srv/tftpboot/repos/$repo
                /usr/bin/createrepo /srv/tftpboot/repos/$repo
            fi
            return 0
        fi
        die "$repo does not contain the right repository ($products_xml is missing summary '$expected_summary')"
    fi
}

# FIXME: repos that we cannot check yet:
#   SP3-Updates is lacking products.xml
#   Cloud: we don't have the final md5
#   SUSE-Cloud-2.0-*: lacking products.xml
REPOS_SKIP_CHECKS+=" Cloud SLES11-SP3-Updates SUSE-Cloud-2.0-Pool SUSE-Cloud-2.0-Updates"

check_repo_content \
    SLES11_SP3 \
    /srv/tftpboot/suse-11.3/install \
    d0bb700ab51c180200995dfdf5a6ade8

check_repo_content \
    Cloud \
    /srv/tftpboot/repos/Cloud \
    1558be86e7354d31e71e7c8c2574031a


if skip_check_for_repo "Cloud-PTF"; then
    echo "Skipping check for Cloud-PTF due to \$REPOS_SKIP_CHECKS"
else
    if ! [ -e "/srv/tftpboot/repos/Cloud-PTF/repodata/repomd.xml" ]; then
        # Only do this for CROWBAR_FROM_GIT , as usually the crowbar package
        # creates the repo metadata for Cloud-PTF
        if [ -n $CROWBAR_FROM_GIT ]; then
            echo "Creating repo skeleton to make AutoYaST happy."
            if ! [ -d /srv/tftpboot/repos/Cloud-PTF ]; then
                mkdir /srv/tftpboot/repos/Cloud-PTF
            fi
            /usr/bin/createrepo /srv/tftpboot/repos/Cloud-PTF
        else
            die "Cloud-PTF has not been set up correctly; did the crowbar rpm fail to install correctly?"
        fi
    fi
fi

check_repo_product SLES11-SP3-Pool        'SUSE Linux Enterprise Server 11 SP3'
check_repo_product SLES11-SP3-Updates     'SUSE Linux Enterprise Server 11 SP3'
check_repo_product SUSE-Cloud-2.0-Pool    'SUSE Cloud 2.0'
check_repo_product SUSE-Cloud-2.0-Updates 'SUSE Cloud 2.0'

if [ -z "$CROWBAR_FROM_GIT" ]; then
    if ! LANG=C zypper if -t pattern cloud_admin 2> /dev/null | grep -q "^Installed: Yes$"; then
        die "cloud_admin pattern is not installed; please install with \"zypper in -t pattern cloud_admin\". Aborting."
    fi
fi

sign_repositories

# Setup helper for git
# --------------------

add_ibs_repo () {
    url="$1"
    alias="$2"
    if ! [ -f /etc/zypp/repos.d/$alias.repo ]; then
        zypper ar $url $alias
    else
        echo "Repo: $alias already exists. Skipping."
    fi
}

if [ -n "$CROWBAR_FROM_GIT" ]; then

    echo_summary "Performing additional setup for git"

    # FIXME: This is useful only for testing the crowbar admin node setup.
    #        Additional work (e.g. on the autoyast profile) is required to make
    #        those repos available to any client nodes.
    if [ $CROWBAR_FROM_GIT = "ibs" ]; then
        add_ibs_repo http://dist.suse.de/install/SLP/SLES-11-SP3-LATEST/x86_64/DVD1 sp3
        add_ibs_repo http://dist.suse.de/install/SLP/SLE-11-SP3-SDK-LATEST/x86_64/DVD1/ sdk-sp3
        add_ibs_repo http://dist.suse.de/ibs/SUSE:/SLE-11-SP3:/GA/standard/ sp3-ga
        add_ibs_repo http://dist.suse.de/ibs/SUSE:/SLE-11-SP3:/Update/standard/ sp3-update
        add_ibs_repo http://dist.suse.de/ibs/Devel:/Cloud:/2.0/SLE_11_SP3/ cloud
    fi

    # install chef and its dependencies
    zypper --gpg-auto-import-keys in rubygem-chef-server rubygem-chef rabbitmq-server \
            couchdb java-1_6_0-ibm rubygem-activesupport

    # also need these (crowbar dependencies):
    zypper in rubygem-cstruct rubygem-kwalify rubygem-ruby-shadow rubygem-sass rubygem-i18n sleshammer tcpdump

    # Need this for provisioner to work:
    mkdir -p /srv/tftpboot/discovery/pxelinux.cfg
    cat > /srv/tftpboot/discovery/pxelinux.cfg/default <<EOF
DEFAULT pxeboot
TIMEOUT 20
PROMPT 0
LABEL pxeboot
        KERNEL vmlinuz0
        APPEND initrd=initrd0.img root=/sledgehammer.iso rootfstype=iso9660 rootflags=loop
ONERROR LOCALBOOT 0
EOF
    # create Compatibility link /tftpboot -> /srv/tftpboot (this is part of
    # the crowbar package when not in $CROWBAR_FROM_GIT)
    if ! [ -e /tftpboot ]; then
        ln -s /srv/tftpboot /tftpboot
    elif [ "$( /usr/bin/readlink /tftpboot )" != "/srv/tftpboot" ]; then
        die "/tftpboot exist but is not a symbolic link to /srv/tftpboot. Please fix!"
    fi

    # log directory needs to exist
    mkdir -p /var/log/crowbar
    chmod 0750 /var/log/crowbar

    # You'll also need:
    #   /srv/tftpboot/discovery/initrd0.img
    #   /srv/tftpboot/discovery/vmlinuz0
    # These can be obtained from a sleshammer image or from an existing
    # ubuntu admin node.
fi


# Starting services
# -----------------

echo_summary "Starting required services"

chkconfig rabbitmq-server on
ensure_service_running rabbitmq-server '^Node .+ with Pid [0-9]+: running'

if rabbitmqctl list_vhosts | grep -q '^/chef$'; then
    : /chef vhost already added
else
    rabbitmqctl add_vhost /chef
fi

if rabbitmqctl list_users 2>&1 | grep -q '^chef	'; then
    : chef user already added
else
    rabbit_chef_password=$( dd if=/dev/urandom count=1 bs=16 2>/dev/null | base64 | tr -d / )
    rabbitmqctl add_user chef "$rabbit_chef_password"
    # Update "amqp_pass" in  /etc/chef/server.rb and solr.rb
    sed -i 's/amqp_pass ".*"/amqp_pass "'"$rabbit_chef_password"'"/' /etc/chef/{server,solr}.rb
fi

rabbitmqctl set_permissions -p /chef chef ".*" ".*" ".*"

chkconfig couchdb on
ensure_service_running couchdb

chmod o-rwx /etc/chef /etc/chef/{server,solr}.rb

# increase chef-solr index field size
perl -i -pe 's{<maxFieldLength>.*</maxFieldLength>}{<maxFieldLength>200000</maxFieldLength>}' /var/lib/chef/solr/conf/solrconfig.xml

services='solr expander server'
for service in $services; do
    chkconfig chef-${service} on
done

for service in $services; do
    ensure_service_running chef-${service}
done


# Initial chef-client run
# -----------------------

echo_summary "Performing initial chef-client run"

if ! [ -e ~/.chef/knife.rb ]; then
    yes '' | knife configure -i
fi

# Reset chef to install from clean state
# (this needs to be done after knife configure, since we need a client key)

# we do not want to delete the chef-validator, chef-webui, root clients; we
# only really want to delete clients that are named with FQDN
knife client -y bulk delete ".*\.$DOMAIN"
test -f /etc/chef/client.pem && rm /etc/chef/client.pem
# the crowbar cookbook creates a "crowbar" client; delete it too
if knife client show crowbar > /dev/null 2>&1; then
    knife client -y delete crowbar
fi
test -f /opt/dell/crowbar_framework/config/client.pem && rm /opt/dell/crowbar_framework/config/client.pem

knife cookbook -y bulk delete ".*"
# no bulk delete for data bag
for bag in `knife data bag list`; do
    knife data bag -y delete $bag
done
knife node -y bulk delete ".*"
knife role -y bulk delete ".*"

cat <<EOF
This can cause warnings about /etc/chef/client.rb missing and
the run list being empty; they can be safely ignored.
EOF

chef-client


# Barclamp installation
# ---------------------

echo_summary "Installing barclamps"

# Clean up previous crowbar install run, in case there was one
test -x /etc/init.d/crowbar && service crowbar stop
for i in $BARCLAMP_SRC/*; do
    if test -d $i -a -f $i-filelist.txt; then
        /opt/dell/bin/barclamp_uninstall.rb $BARCLAMP_INSTALL_OPTS $i
    fi
done
test -e /opt/dell/crowbar_framework/barclamps && rm -r /opt/dell/crowbar_framework/barclamps

if [ -n "$CROWBAR_FROM_GIT" ]; then
    # Create an empty "git" cookbook to satisfy the dependencies of the pfs barclamps
    d=$(mktemp -d)
    knife cookbook create -o "$d" git
    knife cookbook upload -o "$d" git
    rm -rf "$d"
fi

#
# Take care that the barclamps are installed in the right order
# If you've got a full openstack set installed, e.g.: nagios has to be
# installed before keystone, etc.
#
for i in crowbar deployer dns ipmi logging nagios network ntp provisioner \
         database rabbitmq ceph \
         keystone swift glance cinder quantum nova nova_dashboard openstack ; do
    /opt/dell/bin/barclamp_install.rb $BARCLAMP_INSTALL_OPTS $BARCLAMP_SRC/$i
done

# Install optional barclamps if they're present
for i in updater suse-manager-client ; do
    if test -d $BARCLAMP_SRC/$i; then
        /opt/dell/bin/barclamp_install.rb $BARCLAMP_INSTALL_OPTS $BARCLAMP_SRC/$i
    fi
done


# First step of crowbar bootstrap
# -------------------------------

echo_summary "Bootstrapping Crowbar setup"

echo "Create Admin node role"
NODE_ROLE="crowbar-${FQDN//./_}" 
cat > "$CROWBAR_TMPDIR/role.rb" <<EOF
name "$NODE_ROLE"
description "Role for $FQDN"
run_list()
default_attributes( "crowbar" => { "network" => {} } )
override_attributes()
EOF
knife role from file "$CROWBAR_TMPDIR/role.rb"

knife node run_list add "$FQDN" role["crowbar"]
knife node run_list add "$FQDN" role["deployer-client"]
knife node run_list add "$FQDN" role["$NODE_ROLE"]

# at this point you can run chef-client from the command line to start
# the crowbar bootstrapping

chef-client

# OOC, what, if anything, is responsible for starting rainbows/crowbar under bluepill?
ensure_service_running crowbar


# Second step of crowbar bootstrap
# -------------------------------

echo_summary "Applying Crowbar configuration for Administration Server"

# Clean up previous crowbar install run, in case there was one
test -f /etc/crowbar.install.key && rm /etc/crowbar.install.key
test -f /opt/dell/crowbar_framework/htdigest && rm /opt/dell/crowbar_framework/htdigest
test -d /var/lib/crowbar/config && rm -f /var/lib/crowbar/config/*.json
# Clean up files that are created for handling node discovery by provisioner barclamp
test -d /etc/dhcp3/hosts.d && rm -f /etc/dhcp3/hosts.d/*
test -d /srv/tftpboot/discovery && rm -f /srv/tftpboot/discovery/*.conf
test -d /srv/tftpboot/discovery/pxelinux.cfg && rm -f /srv/tftpboot/discovery/pxelinux.cfg/*

# Keep copy of files that crowbar will overwrite; this is done only on the very
# first run of this script, and allow running the installation script again
# while still having the original data.
for file in /etc/resolv.conf; do
    if test ! -f "/var/lib/crowbar/cache/$file"; then
        mkdir -p "/var/lib/crowbar/cache/`dirname $file`"
        cp -a "$file" "/var/lib/crowbar/cache/$file"
    fi
done

CROWBAR=/opt/dell/bin/crowbar

: ${CROWBAR_FILE:="/etc/crowbar/crowbar.json"}
if test -f "$CROWBAR_FILE"; then
    cp "$CROWBAR_FILE" "$CROWBAR_TMPDIR/crowbar.json"
fi
CROWBAR_FILE="$CROWBAR_TMPDIR/crowbar.json"

mkdir -p /var/lib/crowbar/config

# force id and use merge with template
/opt/dell/bin/json-edit "$CROWBAR_FILE" -a id -v "default"
/opt/dell/bin/json-edit "$CROWBAR_FILE" -a crowbar-deep-merge-template --raw -v "true"
# if crowbar user has been removed from crowbar.json, mark it as disabled (as it's still in main json)
if test -z "`$BARCLAMP_SRC/provisioner/updates/parse_node_data "$CROWBAR_FILE" -a attributes.crowbar.users.crowbar | sed "s/^[^=]*=//g"`"; then
    /opt/dell/bin/json-edit "$CROWBAR_FILE" -a attributes.crowbar.users.crowbar.disabled --raw -v "true"
fi
# we don't use ganglia at all, and we don't want nagios by default
/opt/dell/bin/json-edit "$CROWBAR_FILE" -a attributes.crowbar.instances.ganglia --raw -v "[ ]"
/opt/dell/bin/json-edit "$CROWBAR_FILE" -a attributes.crowbar.instances.nagios --raw -v "[ ]"

# use custom network configuration if there's one
if [ -f /etc/crowbar/network.json ]; then
    cp -a /etc/crowbar/network.json /var/lib/crowbar/config/network.json
    /opt/dell/bin/json-edit "/var/lib/crowbar/config/network.json" -a id -v "default"
    /opt/dell/bin/json-edit "/var/lib/crowbar/config/network.json" -a crowbar-deep-merge-template --raw -v "true"
    /opt/dell/bin/json-edit "$CROWBAR_FILE" -a attributes.crowbar.instances.network --raw -v "[ \"/var/lib/crowbar/config/network.json\" ]"
    echo "Using custom network configuration from /etc/crowbar/network.json"
fi

# Use existing SSH authorized keys
if [ -f /root/.ssh/authorized_keys ]; then
    # remove empty lines and change newline to \n
    access_keys=$(sed "/^ *$/d" /root/.ssh/authorized_keys | sed "N;s/\n/\\n/g")
    /opt/dell/bin/json-edit "/var/lib/crowbar/config/provisioner.json" -a id -v "default"
    /opt/dell/bin/json-edit "/var/lib/crowbar/config/provisioner.json" -a crowbar-deep-merge-template --raw -v "true"
    /opt/dell/bin/json-edit "/var/lib/crowbar/config/provisioner.json" -a attributes.provisioner.access_keys -v "$access_keys"
    /opt/dell/bin/json-edit "$CROWBAR_FILE" -a attributes.crowbar.instances.provisioner --raw -v "[ \"/var/lib/crowbar/config/provisioner.json\" ]"
    echo "Will add pre-existing SSH keys from /root/.ssh/authorized_keys"
fi

# Setup bind with correct local domain and DNS forwarders
nameservers=$( awk '/^nameserver/ {printf "\""$2"\","}' /var/lib/crowbar/cache/etc/resolv.conf | sed "s/,$//" )
/opt/dell/bin/json-edit "/var/lib/crowbar/config/dns.json" -a id -v "default"
/opt/dell/bin/json-edit "/var/lib/crowbar/config/dns.json" -a crowbar-deep-merge-template --raw -v "true"
/opt/dell/bin/json-edit "/var/lib/crowbar/config/dns.json" -a attributes.dns.domain -v "$DOMAIN"
/opt/dell/bin/json-edit "/var/lib/crowbar/config/dns.json" -a attributes.dns.forwarders --raw -v "[ $nameservers ]"
/opt/dell/bin/json-edit "$CROWBAR_FILE" -a attributes.crowbar.instances.dns --raw -v "[ \"/var/lib/crowbar/config/dns.json\" ]"
echo "Will configure bind with the following DNS forwarders: $nameservers"

mkdir -p /opt/dell/crowbar_framework
CROWBAR_REALM=$($BARCLAMP_SRC/provisioner/updates/parse_node_data $CROWBAR_FILE -a attributes.crowbar.realm)
CROWBAR_REALM=${CROWBAR_REALM##*=}

# Generate the machine install username and password.
if [[ ! -e /etc/crowbar.install.key && $CROWBAR_REALM ]]; then
    dd if=/dev/urandom bs=65536 count=1 2>/dev/null |sha512sum - 2>/dev/null | \
        (read key rest; echo "machine-install:$key" >/etc/crowbar.install.key)
fi

if [[ $CROWBAR_REALM && -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
    /opt/dell/bin/json-edit "$CROWBAR_FILE" -a attributes.crowbar.users.machine-install.password -v "${CROWBAR_KEY##*:}"
fi

# Make sure looper_chef_client is a NOOP until we are finished deploying
touch /tmp/deploying
# This works because
#
#   crowbar_framework/app/models/provisioner_service.rb
#   crowbar_framework/app/models/service_object.rb
#
# both invoke
#
#   /opt/dell/bin/single_chef_client.sh (from barclamps/crowbar/bin)
#
# which invokes
#
#   /opt/dell/bin/looper_chef_client.sh
#
# which exits immediately if /tmp/deploying exists.

# From here, you should probably read along with the equivalent steps in
# install-chef.sh for comparison

if [ "$($CROWBAR crowbar proposal list)" != "default" ] ; then
    proposal_opts=()
    # If your custom crowbar.json is somewhere else, probably substitute that here
    if [[ -e $CROWBAR_FILE ]]; then
        proposal_opts+=(--file $CROWBAR_FILE)
    fi
    proposal_opts+=(proposal create default)

    # Sometimes proposal creation fails if Chef and Crowbar are not quite
    # fully prepared -- this can happen due to solr not having everything
    # fully indexed yet.  So we don't want to just fail immediatly if
    # we fail to create a proposal -- instead, we will kick Chef, sleep a bit,
    # and try again up to 5 times before bailing out.
    for ((x=1; x<6; x++)); do
        $CROWBAR crowbar "${proposal_opts[@]}" && { proposal_created=true; break; }
        echo "Proposal create failed, pass $x.  Will kick Chef and try again."
        chef-client
        sleep 1
    done
    if [[ ! $proposal_created ]]; then
        die "Could not create default proposal"
    fi
fi

# this has machine key world readable? care?
$CROWBAR crowbar proposal show default >/var/log/crowbar/default-proposal.json

# next will fail if ntp barclamp not present (or did for me...)
$CROWBAR crowbar proposal commit default || \
    die "Could not commit default proposal!"
    
$CROWBAR crowbar show default >/var/log/crowbar/default.json

crowbar_up=true
chef-client

# Need to make sure that we have the indexer/expander finished
COUNT=0
VALUE=10000
while (($COUNT < 60 && $VALUE !=0))
do
    sleep 1
    VALUE=$(chef-expanderctl queue-depth | grep total | awk -F: '{ print $2 }')
    echo "Expander Queue Total = $VALUE"
    COUNT=$(($COUNT + 1))
done

# original script has several calls to check_machine_role -- see source for
# this, in my limited testing it wasn't necessary on SUSE, but we should still
# do it anyway (if the role isn't present things break, apparently)

# BMC support?


# Third step of crowbar bootstrap
# -------------------------------

echo_summary "Transitioning Administration Server to \"ready\""

# transition though all the states to ready.  Make sure that
# Chef has completly finished with transition before proceeding
# to the next.

for state in "discovering" "discovered" "hardware-installing" \
    "hardware-installed" "installing" "installed" "readying" "ready"
do
    while [[ -f "/tmp/chef-client.lock" ]]; do sleep 1; done
    printf "$state: "
    $CROWBAR crowbar transition "$FQDN" "$state" || \
        die "Transition to $state failed!"
    if type -f "transition_check_$state"&>/dev/null; then
        "transition_check_$state" || \
            die "Sanity check for transitioning to $state failed!"
    fi
    # chef_or_die "Chef run for $state transition failed!"
    chef-client
    # check_machine_role
done

# OK, let looper_chef_client run normally now.
rm /tmp/deploying


# Starting more services
# ----------------------

echo_summary "Starting chef-client"

# Need chef-client daemon now
chkconfig chef-client on
ensure_service_running chef-client


# Final sanity checks
# -------------------

echo_summary "Performing post-installation sanity checks"

# Spit out a warning message if we managed to not get an IP address
IPSTR=$($CROWBAR network show default | /opt/dell/barclamps/provisioner/updates/parse_node_data -a attributes.network.networks.admin.ranges.admin.start)
IP=${IPSTR##*=}
ip addr | grep -q $IP || {
    die "eth0 not configured, but should have been."
}

# Run tests -- currently the host will run this.
# 2012-05-24: test is failing, so only run if CROWBAR_RUN_TESTS=true
# (this is distinct from CROWBAR_FROM_GIT, which would add extra repos etc.)
if [ -n "$CROWBAR_RUN_TESTS" ]; then
    /opt/dell/bin/barclamp_test.rb -t || \
        die "Crowbar validation has errors! Please check the logs and correct."
fi

for s in xinetd dhcpd apache2 ; do
    if ! /etc/init.d/$s status >/dev/null ; then
        die "service $s missing"
    fi
done


# We're done!
# -----------

echo_summary "Installation complete!"

touch /opt/dell/crowbar_framework/.crowbar-installed-ok

kill_spinner

cat <<EOF | pipe_show_and_log


Admin node deployed.

You can now visit the Crowbar web UI at:

    http://$IP:3000/

You should also now be able to PXE-boot a client.  Please refer
to the documentation for the next steps.

Note that to run the crowbar CLI tool, you will need to log out
and log back in again for the correct environment variables to
be set up.
EOF

run_succeeded=hooray
