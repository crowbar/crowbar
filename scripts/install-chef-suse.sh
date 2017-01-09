#!/bin/bash
#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This script is called after being installed by the Crowbar RPM from the SUSE
# OpenStack Cloud ISO. In this context, it is expected that all other required
# repositories (eg. SLES, Updates) are already set up, with the required files
# in place.
#
# For development and testing use, call the script with the '--from-git'
# option. Use the appropriate dev VM and follow the corresponding setup
# instructions.

set -e

crowbar_install_dir=/var/lib/crowbar/install
installation_steps=$crowbar_install_dir/installation_steps

function set_step () {
    local step=$1
    echo "$step $(date -Iseconds)" >> $installation_steps
}

touch $crowbar_install_dir/crowbar_installing

usage () {
    # do not document --from-git option; it's for developers only
    cat <<EOF
`basename $0` [-h|--help] [-v|--verbose] [-d|--debug] [-c|--crowbar]

Install Crowbar on administration server.
EOF
    exit
}

# Store the commandline before we process it (and destroy it)
commandline="$*"

while test $# -gt 0; do
    case "$1" in
        -h|--help|--usage|-\?) usage ;;
        -v|--verbose) CROWBAR_VERBOSE=1 ;;
        -d|--debug) CROWBAR_DEBUG=1 ;;
        -c|--crowbar) CROWBAR_WIZARD_MODE=1 && CROWBAR_VERBOSE=1 ;;
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

if [ -z "$HOME" ]; then
    echo "The HOME environment variable is not set."
    echo ""
    exit 1
fi

if [[ $USER != "root" ]]; then
    echo "The environment variables are not correct. Please use don't use 'su' to get root privileges."
    echo ""
    exit 1
fi

if [ -n "$CROWBAR_DEBUG" ]; then
    BARCLAMP_INSTALL_OPTS="--debug"
    chef_client="chef-client -l debug"
else
    BARCLAMP_INSTALL_OPTS=""
    chef_client="chef-client"
fi

if [ -n "$CROWBAR_FROM_GIT" ]; then
    BARCLAMP_INSTALL_OPTS="$BARCLAMP_INSTALL_OPTS --force"
    : ${CROWBAR_JSON:=/root/crowbar/crowbar.json}
    : ${BARCLAMP_SRC:=/root/crowbar/barclamps/}
    mkdir -p /opt/dell/bin
    for tool in extra/* change-image/dell/*; do
        if [ -f $tool -a -x $tool ]; then
            install -p -m 0755 $tool /opt/dell/bin/
        fi
    done
else
    BARCLAMP_INSTALL_OPTS="$BARCLAMP_INSTALL_OPTS --rpm"
fi

LOGFILE=/var/log/crowbar/install.log
mkdir -p "`dirname "$LOGFILE"`"

: ${BARCLAMP_SRC:="/opt/dell/barclamps/"}

run_succeeded=

is_ses () {
    [ -d /opt/dell/barclamps/ses ]
}


DIALOG_TITLE=" SUSE OpenStack Cloud 6 "

if is_ses; then
    DIALOG_TITLE=" SUSE Enterprise Storage "
fi

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
        dialog --keep-tite --title "$DIALOG_TITLE" --textbox -- $t $(($(wc -l <$t)+4)) 75 >&3
        rm -f $t
        dialog --clear >&3
    fi
    tee -a /dev/fd/4 >&3
}

# Draw a spinner so the user knows something is happening
spinner () {
    local delay=0.75
    local spinstr='/-\|'
    local msg="$@"

    # reset exit handler
    trap "exit" EXIT

    printf " " >&3
    while [ true ]; do
        local temp=${spinstr#?}
        if use_dialog; then
            printf "\n%s [%c]" "$msg... " "$spinstr" | dialog \
                --keep-tite --title "$DIALOG_TITLE" \
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
            echo -n -e "$@..." >&3
            if use_dialog; then
                echo -n -e "\n$@... " | dialog --title "$DIALOG_TITLE" --progressbox 5 70 >&3
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
        post_fail_handler
        kill_spinner_with_failed
        cat <<EOF | pipe_show_and_log

Crowbar installation terminated prematurely.  Please examine the above
output or $LOGFILE for clues as to what went wrong.
You should also check the Installation Manual, in
particular the Troubleshooting section.  Note that this script can
safely be re-run multiple times if required.
EOF
    else
        kill_spinner
    fi
}

trap exit_handler EXIT
trap post_fail_handler INT

post_fail_handler ()
{
    touch $crowbar_install_dir/crowbar-install-failed
    rm -f $crowbar_install_dir/crowbar_installing
}

function reset_crowbar()
{
    rm -f $crowbar_install_dir/crowbar-install-failed
    rm -f $installation_steps
    su -s /bin/sh - crowbar sh -c "cd /opt/dell/crowbar_framework && RAILS_ENV=production bin/rake db:cleanup"
}

# Real work starts here
# ---------------------

echo "`date` $0 started with args: $commandline"

CROWBAR_TMPDIR=$(mktemp -d --tmpdir crowbar-install-XXXXXX)

ensure_service_running () {
    service="$1"
    if service $service status >/dev/null; then
        echo "$service is already running - no need to start."
    else
        service $service start
        sleep 4
    fi
}

wait_for_crowbar ()
{
    local crowbar_up=0
    for ((x=1; x<10; x++)); do
        nc -z localhost 3000 && {
            crowbar_up=1
            break
        }
        sleep 2
    done
    if [ $crowbar_up -ne 1 ]; then
        die "Crowbar service is not running. Please check $LOGFILE."
    fi
}

if [ -f $crowbar_install_dir/crowbar-installed-ok ]; then
    run_succeeded=already_before

cat <<EOF | pipe_show_and_log
Aborting: Administration Server is already deployed.

If you want to run the installation script again,
then please remove the following file:

    $crowbar_install_dir/crowbar-installed-ok
EOF
    exit 1
fi

reset_crowbar

FQDN=$(hostname -f 2>/dev/null);
DOMAIN=$(hostname -d 2>/dev/null);
IPv4_addr=$( getent ahosts $FQDN 2>/dev/null | awk '{ if ($1 !~ /:/) { print $1; exit } }' )
# Set no_proxy for localhost, the FQDN and the hostname, so chef will not use
# the proxy for them.
export no_proxy="$no_proxy,localhost,$FQDN,$IPv4_addr"

# Sanity checks
# -------------

echo_summary "Performing sanity checks"

if [ -n "$SSH_CONNECTION" -a -z "$STY" ] && [ -z "$CROWBAR_WIZARD_MODE" ]; then
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

if [ -n "$CROWBAR_FROM_GIT" ]; then
    zypper -n in ruby2.1-rubygem-json-1_7 createrepo
fi

json_edit=/opt/dell/bin/json-edit
json_read () {
    file="$1" attr="$2"
    if [ "$file" = '-' ]; then
        /opt/dell/bin/json-read -a "$attr"
    elif [ -f "$file" ]; then
        /opt/dell/bin/json-read "$file" -a "$attr"
    fi | sed "s/^[^=]*=//g"
}

if [ -f /etc/crowbar/provisioner.json ]; then
    PROVISIONER_JSON=/etc/crowbar/provisioner.json
elif [ -n "$CROWBAR_FROM_GIT" -a -f /root/crowbar/provisioner.json ]; then
    PROVISIONER_JSON=/root/crowbar/provisioner.json
fi

if [ -n "$IPv4_addr" ]; then
    if [ -f /etc/crowbar/network.json ]; then
        NETWORK_JSON=/etc/crowbar/network.json
    elif [ -n "$CROWBAR_FROM_GIT" ]; then
        NETWORK_JSON=$BARCLAMP_SRC/network/chef/data_bags/crowbar/template-network.json
    else
        NETWORK_JSON=/opt/dell/chef/data_bags/crowbar/template-network.json
    fi
    # allow using old json files # TODO: drop in 2017
    sed -i -e 's/bc-template-network/template-network/' $NETWORK_JSON

    if ! /opt/dell/bin/network-json-validator --admin-ip "$IPv4_addr" $NETWORK_JSON; then
        die "Failed to validate network.json configuration. Please check and fix with yast2 crowbar. Aborting."
    fi
fi
# output details, that will make remote debugging via bugzilla much easier
# for us
/usr/bin/zypper -n lr -d  || :
/bin/rpm -qV crowbar || :
/usr/bin/lscpu  || :
/bin/df -h  || :
/usr/bin/free -m || :
/bin/ls -la /srv/tftpboot/suse-*/*/{install/,repos/,repos/Cloud/} || :

if [ -f /opt/dell/chef/cookbooks/provisioner/templates/default/autoyast.xml.erb ]; then
    # The autoyast profile might not exist yet when CROWBAR_FROM_GIT is enabled
    /usr/bin/grep media_url /opt/dell/chef/cookbooks/provisioner/templates/default/autoyast.xml.erb
fi

check_or_create_ptf_repository () {
    version="$1"
    arch="$2"
    repo="$3"

    if skip_check_for_repo "$repo"; then
        echo "Skipping check for $repo ($version) due to \$REPOS_SKIP_CHECKS"
    else
        if ! [ -e "/srv/tftpboot/suse-$version/$arch/repos/$repo/repodata/repomd.xml" ]; then
            # Only do this for CROWBAR_FROM_GIT, as usually the crowbar package
            # creates the repo metadata for PTF
            if [ -n $CROWBAR_FROM_GIT ]; then
                echo "Creating repo skeleton to make AutoYaST happy."
                if ! [ -d /srv/tftpboot/suse-$version/$arch/repos/$repo ]; then
                    mkdir /srv/tftpboot/suse-$version/$arch/repos/$repo
                fi
                /usr/bin/createrepo /srv/tftpboot/suse-$version/$arch/repos/$repo
            else
                die "$repo ($version / $arch) has not been set up correctly; \
                    did the crowbar rpm fail to install correctly?"
            fi
        fi
    fi
}

create_gpg_key () {
    # To sign the repositories the admin node needs a gpg key - create one, if
    # it's not present

    if ! [[ -n $(gpg --list-secret-keys) ]]; then
        # no secret key found - create one
        # create batch file for automated key generation
        echo "Generating GPG key that will be used to sign repositories"
        cat > $CROWBAR_TMPDIR/SUSE-key.batch <<EOF
Key-Type: RSA
Key-Length: 2048
Name-Real: PTF repository signing key
Name-Comment: autogenerated
Name-Email: nobody@$FQDN
Expire-Date: 0
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
    version="$1"
    arch="$2"
    repo="$3"

    create_gpg_key

    tftpdir=/srv/tftpboot/suse-$version/$arch/repos/$repo
    if [ -f $tftpdir/repodata/repomd.xml ]; then
        if [ ! -f $tftpdir/repodata/repomd.xml.asc -o \
            ! -f $tftpdir/repodata/repomd.xml.key ]; then
        echo "Signing $repo ($version / $arch) repository"
        gpg -a --detach-sign $tftpdir/repodata/repomd.xml
        gpg -a --export > $tftpdir/repodata/repomd.xml.key
        else
        echo "$repo ($version / $arch) repository is already signed"
        fi
    fi
}

for repos_check in \
    "$(dirname $0)/repos-check-suse" \
    "/usr/lib/suse-cloud/repos-check"; do
    if [ -f $repos_check ]; then
        . $repos_check
        break
    fi
done

if [ -z "$(type -t skip_check_for_repo)" ]; then
    die "Broken setup: no repos-check helper library"
fi

#supported_arches="x86_64 ppc64le"
supported_arches="x86_64"
supported_arches_ses="x86_64"

if is_ses; then
    product_arches="$supported_arches_ses"
else
    product_arches="$supported_arches"
fi

for arch in $product_arches; do
    # Checks for SLE12 SP1 medias
    MEDIA=/srv/tftpboot/suse-12.1/$arch/install

    # Only x86_64 is truly mandatory; other architectures are only checked
    # if they exist
    if [ ! -f $MEDIA/content -a $arch != "x86_64" ]; then
        continue
    fi

    check_media_links $MEDIA

    if [ -f $MEDIA/content ] && egrep -q "REPOID.*/suse-openstack-cloud-deps/" $MEDIA/content; then
        echo "Detected SUSE OpenStack Cloud Deps media."
    else
        check_media_content \
            SLES12-SP1 \
            $MEDIA \
            #b52c0f2b41a6a10d49cc89edcdc1b13d

        if ! is_ses; then
            check_media_content \
                Cloud \
                /srv/tftpboot/suse-12.1/$arch/repos/Cloud \
                #1558be86e7354d31e71e7c8c2574031a
        fi
    fi
done

if [ -z "$CROWBAR_FROM_GIT" ]; then
    pattern=patterns-cloud-admin
    pattern_short=cloud_admin
    if is_ses; then
        pattern=patterns-ses-admin
        pattern_short=ses_admin
    fi
    if ! rpm -q --whatprovides $pattern &> /dev/null; then
        die "$pattern package is not installed; please install with \"zypper in -t pattern $pattern_short\" or \"zypper in $pattern\". Aborting."
    fi
fi


for arch in $supported_arches; do
    check_or_create_ptf_repository 12.1 $arch PTF

    # Currently we only sign the PTF repository
    sign_repositories 12.1 $arch PTF
done

# Setup helper for git
# --------------------

add_ibs_repo () {
    url="$1"
    alias="$2"
    if ! [ -f /etc/zypp/repos.d/$alias.repo ]; then
        zypper -n ar $url $alias
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
        add_ibs_repo http://dist.suse.de/install/SLP/SLE-12-SP1-Server-GM/x86_64/DVD1/ sle12sp1
        add_ibs_repo http://euklid.suse.de/mirror/SuSE/build.suse.de/SUSE/Updates/SLE-SERVER/12-SP1/x86_64/update/ sle12sp1-update
        add_ibs_repo http://dist.suse.de/install/SLP/SLE-12-SP1-SDK-GM/x86_64/DVD1/ sle12sp1-sdk
        add_ibs_repo http://euklid.suse.de/mirror/SuSE/build.suse.de/SUSE/Updates/SLE-SDK/12-SP1/x86_64/update/ sle12sp1-sdk-update
        add_ibs_repo http://dist.suse.de/ibs/Devel:/Cloud:/6/SLE_12_SP1/ cloud
    fi

    # install chef and its dependencies
    zypper -n --gpg-auto-import-keys in ruby2.1-rubygem-chef-server ruby2.1-rubygem-chef \
        rabbitmq-server couchdb ruby2.1-rubygem-activesupport

    # also need these (crowbar dependencies):
    zypper -n in sleshammer tcpdump

    # log directory needs to exist
    mkdir -p /var/log/crowbar
    chmod 0750 /var/log/crowbar

    # You'll also need:
    #   /srv/tftpboot/discovery/$arch/initrd0.img
    #   /srv/tftpboot/discovery/$arch/vmlinuz0
    # These can be obtained from a sleshammer image or from an existing
    # ubuntu admin node.
fi

set_step "pre_sanity_checks"


# Starting services
# -----------------

echo_summary "Starting required services"

# Add frame_max setting as environment variable, because it's just a workaround
if [ $(rpm -q --qf "%{version}" rubygem-chef | cut -d . -f 1) -ne 10 ]; then
    die "Please remove the frame_max setting in rabbitmq."
fi

if test -f /etc/rabbitmq/rabbitmq-env.conf && \
    grep -q ^SERVER_START_ARGS= /etc/rabbitmq/rabbitmq-env.conf && \
    ! grep -q '^SERVER_START_ARGS=.*-rabbit frame_max 0' /etc/rabbitmq/rabbitmq-env.conf; then
    die "SERVER_START_ARGS already defined in /etc/rabbitmq/rabbitmq-env.conf, without \"-rabbit frame_max 0\""
elif ! ( test -f /etc/rabbitmq/rabbitmq-env.conf && \
    grep -q ^SERVER_START_ARGS= /etc/rabbitmq/rabbitmq-env.conf ); then
    echo '# Workaround for bunny version required by chef not supporting frames' >> /etc/rabbitmq/rabbitmq-env.conf
    echo '# https://bugzilla.suse.com/show_bug.cgi?id=910815' >> /etc/rabbitmq/rabbitmq-env.conf
    echo 'SERVER_START_ARGS="-rabbit frame_max 0"' >> /etc/rabbitmq/rabbitmq-env.conf
fi

# Write default RabbitMQ configuration (if the package doesn't provide one).
if [ ! -f /etc/rabbitmq/rabbitmq.config ] ; then
    cat << EOF > /etc/rabbitmq/rabbitmq.config
[
 {rabbit,
  [{disk_free_limit, 50000000}]
 }
].
EOF
fi

chkconfig rabbitmq-server on
ensure_service_running rabbitmq-server

chkconfig couchdb on
ensure_service_running couchdb

# chef-server is way too verbose in :info, with nothing useful in the log
sed -i 's/log_level  *:.*/log_level :warn/' /etc/chef/server.rb
# increase chef-solr index field size
perl -i -pe 's{<maxFieldLength>.*</maxFieldLength>}{<maxFieldLength>200000</maxFieldLength>}' /var/lib/chef/solr/conf/solrconfig.xml

services='solr expander server'
for service in $services; do
    chkconfig chef-${service} on
done

for service in $services; do
    ensure_service_running chef-${service}
done

set_step "run_services"


# Initial chef-client run
# -----------------------

echo_summary "Performing initial chef-client run"

# Stop chef-client daemon to avoid interferences
service chef-client status &> /dev/null && service chef-client stop

if ! [ -e ~/.chef/knife.rb -a -e ~/.chef/root.pem ]; then
    if knife client list | grep -q "^ *root$"; then
        knife client delete --yes root
    fi
    knife configure \
    --initial \
    --yes \
    --defaults \
    --user root \
    --admin-client-name chef-webui \
    --admin-client-key /etc/chef/webui.pem \
    --validation-client-name chef-validator \
    --validation-key /etc/chef/validation.pem \
    --repository ""
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

cat > /etc/chef/client.rb <<EOF
chef_server_url 'http://$IPv4_addr:4000'
enable_reporting false
EOF

$chef_client

set_step "initial_chef_client"


# Barclamp installation
# ---------------------

echo_summary "Installing barclamps"

for i in $BARCLAMP_SRC/*; do
    if test -d $i -a -f $i-filelist.txt; then
        /opt/dell/bin/barclamp_uninstall.rb $BARCLAMP_INSTALL_OPTS $i
    fi
done

if [ -n "$CROWBAR_FROM_GIT" ]; then
    # Create empty "git" and "nagios" cookbooks to satisfy the dependencies of
    # the barclamps (PFS + monitor)
    d=$(mktemp -d)
    knife cookbook create -o "$d" git
    knife cookbook upload -o "$d" git
    knife cookbook create -o "$d" nagios
    # the nagios::common recipe is included from other cookbooks
    touch "$d/nagios/recipes/common.rb"
    knife cookbook upload -o "$d" nagios
    rm -rf "$d"
    $json_edit "$CROWBAR_JSON" \
        -a attributes.crowbar.instances.nagios \
        --raw -v "[ ]"

    # Some barclamps depend on the "pfsdeps" view. Fake it, to make the webui
    # work for those.
    install -m 0755 -d /opt/dell/crowbar_framework/app/views/barclamp/git/
    touch /opt/dell/crowbar_framework/app/views/barclamp/git/_pfsdeps.html.haml
fi

required_components="core"
if is_ses ; then
    required_components+=" ses ceph"
else
    required_components+=" openstack ha ceph"
fi

/opt/dell/bin/barclamp_install.rb $BARCLAMP_INSTALL_OPTS $required_components

# Install optional components if they're present
if test -d $BARCLAMP_SRC/hyperv; then
    /opt/dell/bin/barclamp_install.rb $BARCLAMP_INSTALL_OPTS hyperv
fi

set_step "barclamp_install"

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

$chef_client

# OOC, what, if anything, is responsible for starting rainbows/crowbar under bluepill?
ensure_service_running crowbar

set_step "bootstrap_crowbar_setup"


# Second step of crowbar bootstrap
# -------------------------------

echo_summary "Applying Crowbar configuration for Administration Server"

# Clean up previous crowbar install run, in case there was one
# Note that we don't stop ntpd since it's possibly used later on, nor
# named/dnsmasq since they might be required to resolve DNS
for service in dhcpd nfsserver; do
    service $service status &> /dev/null && service $service stop
done
test -f /etc/crowbar.install.key && rm /etc/crowbar.install.key
test -f /opt/dell/crowbar_framework/htdigest && \
    rm /opt/dell/crowbar_framework/htdigest
test -d /var/lib/crowbar/config && rm -f /var/lib/crowbar/config/*.json
# Clean up files that are created for handling node discovery by provisioner barclamp
test -d /etc/dhcp3/hosts.d && rm -f /etc/dhcp3/hosts.d/*
for arch in $supported_arches; do
    test -d /srv/tftpboot/discovery/$arch/efi && \
        rm -f /srv/tftpboot/discovery/$arch/efi/*.conf
    test -d /srv/tftpboot/discovery/$arch/bios/pxelinux.cfg && \
        rm -f /srv/tftpboot/discovery/$arch/bios/pxelinux.cfg/*
done

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

# Use custom configurations from /etc where they exist and are permitted.
# These are treated as read-only and copied into /var/lib/crowbar/config
# for modification.
mkdir -p /var/lib/crowbar/config
for bc in crowbar dns network provisioner ntp; do
    # Use CROWBAR_JSON, NETWORK_JSON etc. if they are set above
    json_var_name=$(echo "${bc}_json" | tr a-z A-Z )
    custom_json="${!json_var_name:-/etc/crowbar/$bc.json}"
    json_to_merge=/var/lib/crowbar/config/$bc.json
    if [ -f $custom_json ]; then
        cp -a $custom_json $json_to_merge
        echo "Using custom $bc configuration from $custom_json"
    fi
    # Make sure that from now on we use the modifiable version
    declare $json_var_name=$json_to_merge
done

# if crowbar user has been removed from crowbar.json, mark it as disabled (as it's still in main json)
if test -z "`json_read "$CROWBAR_JSON" attributes.crowbar.users.crowbar`"; then
    $json_edit "$CROWBAR_JSON" \
        -a attributes.crowbar.users.crowbar.disabled \
        --raw -v "true"
fi
# we don't use ganglia at all, and we don't want nagios by default
$json_edit "$CROWBAR_JSON" \
    -a attributes.crowbar.instances.ganglia \
    --raw -v "[ ]"
$json_edit "$CROWBAR_JSON" -n \
    -a attributes.crowbar.instances.nagios \
    --raw -v "[ ]"

# Use existing SSH authorized keys
if [ -f /root/.ssh/authorized_keys ]; then
    # remove empty lines and change newline to \n
    access_keys=$(sed "/^ *$/d" /root/.ssh/authorized_keys | sed "N;s/\n/\\n/g")
    provisioner_keys=$(json_read "$PROVISIONER_JSON" \
        attributes.provisioner.access_keys)
    if [ ! -f "$PROVISIONER_JSON" -o -z "$provisioner_keys" ]
    then
        echo "Will add pre-existing SSH keys from /root/.ssh/authorized_keys"
        $json_edit "$PROVISIONER_JSON" \
            -a attributes.provisioner.access_keys \
            -v "$access_keys"
    fi
fi

# Use current time zone
(
    . /etc/sysconfig/clock || : Ignoring missing /etc/sysconfig/clock
    if [ -n "$TIMEZONE" ]; then
        echo "Will use $TIMEZONE timezone for node installation"
        $json_edit "$PROVISIONER_JSON" \
            -a attributes.provisioner.timezone \
            -v "$TIMEZONE"
    fi
)

# Setup bind with correct local domain and DNS forwarders
$json_edit "$DNS_JSON" -n -a attributes.dns.domain -v "$DOMAIN"
custom_forwarders="$( json_read "$DNS_JSON" attributes.dns.forwarders )"
if [ -n "$custom_forwarders" ]; then
    echo "bind will use forwarders from $DNS_JSON: $custom_forwarders"
else
    nameservers=$( awk '/^nameserver/ {printf "\""$2"\","}' /var/lib/crowbar/cache/etc/resolv.conf | sed "s/,$//" )
    $json_edit "$DNS_JSON" \
        -a attributes.dns.forwarders --raw \
        -v "[ $nameservers ]"
    echo "bind will use the following DNS forwarders: $nameservers"
fi

custom_ntp_servers="$( json_read "$NTP_JSON" attributes.ntp.external_servers )"
if [ -z "$custom_ntp_servers" ]; then
    # Use existing ntp servers, if any.  We prefer servers in
    # /etc/ntp.conf over servers currently used by a running ntpd,
    # since (a) configuring ntp.conf is a more deliberate manual
    # process than updating peers in a running ntpd (which can happen
    # automatically e.g. via /etc/netconfig.d/ntp-runtime), and as
    # such should be considered more authoritative, and (b) querying
    # the running configuration will only give us canonical hostnames,
    # not round-robin DNS hostnames for NTP server pools.
    echo "Auto-detecting existing ntp servers from /etc/ntp.conf ..."
    ntp_servers="$( awk '/^server / && ! / 127\./ { print $2 }' < /etc/ntp.conf )"
    if [ -z "$ntp_servers" ]; then
        echo "Auto-detecting existing ntp servers from ntpd ..."
        ntp_servers="$( ntpq -wp | tail -n +3 | cut -c2- | grep '^[^ ]' | awk '! /LOCL/ { print $1 }' )"
    fi
    if [ -n "$ntp_servers" ]; then
        ntp_servers="${ntp_servers%$'\n'}" # trim trailing newline
        ntp_servers="\"${ntp_servers//$'\n'/\", \"}\"" # double-quote and comma-delimit
        $json_edit "$NTP_JSON" \
            -a attributes.ntp.external_servers --raw \
            -v "[ $ntp_servers ]"
        echo "ntp will use the following servers: $ntp_servers"
    fi
fi

for bc in crowbar dns network provisioner ntp; do
    json_to_merge=/var/lib/crowbar/config/$bc.json
    [ -f $json_to_merge ] || continue

    $json_edit $json_to_merge -a id -v "default"
    $json_edit $json_to_merge -a crowbar-deep-merge-template --raw -v "true"
    if [ $bc != crowbar ]; then
        $json_edit "$CROWBAR_JSON" \
            -a attributes.crowbar.instances.$bc \
            --raw -v "[ \"$json_to_merge\" ]"
    fi
done

if is_ses; then
    $json_edit "$CROWBAR_JSON" \
        -a attributes.crowbar.realm \
        -v 'SUSE Enterprise Storage Crowbar Admin Server'
fi

mkdir -p /opt/dell/crowbar_framework
CROWBAR_REALM=$(json_read "$CROWBAR_JSON" attributes.crowbar.realm)

# Generate the machine install username and password.
if [[ ! -e /etc/crowbar.install.key && $CROWBAR_REALM ]]; then
    dd if=/dev/urandom bs=65536 count=1 2>/dev/null | \
        sha512sum - 2>/dev/null | \
        (read key rest; echo "machine-install:$key" >/etc/crowbar.install.key)
fi

if [[ $CROWBAR_REALM && -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(</etc/crowbar.install.key)
    $json_edit "$CROWBAR_JSON" \
        -a attributes.crowbar.users.machine-install.password \
        -v "${CROWBAR_KEY##*:}"
fi

# Make sure looper_chef_client is a NOOP until we are finished deploying
touch /var/run/crowbar/deploying
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
# which exits immediately if /var/run/crowbar/deploying exists.

wait_for_crowbar

# From here, you should probably read along with the equivalent steps in
# install-chef.sh for comparison

if [ "$($CROWBAR crowbar proposal list)" != "default" ] ; then
    proposal_opts=()
    # If your custom crowbar.json is somewhere else, probably substitute that here
    if [[ -e $CROWBAR_JSON ]]; then
        proposal_opts+=(--file $CROWBAR_JSON)
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
        $chef_client
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

$CROWBAR crowbar proposal show default >/var/log/crowbar/default.json

crowbar_up=true
$chef_client

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

set_step "apply_crowbar_config"


# Third step of crowbar bootstrap
# -------------------------------

echo_summary "Transitioning Administration Server to \"ready\""

# transition though all the states to ready.  Make sure that
# Chef has completly finished with transition before proceeding
# to the next.

for state in "discovering" "discovered" "hardware-installing" \
    "hardware-installed" "installing" "installed" "readying" "ready"
do
    while [[ -f "/var/run/crowbar/chef-client.lock" ]]; do sleep 1; done

    printf "$state: "
    $CROWBAR crowbar transition "$FQDN" "$state" || \
        die "Transition to $state failed!"

    if type -f "transition_check_$state"&>/dev/null; then
        "transition_check_$state" || \
            die "Sanity check for transitioning to $state failed!"
    fi

    if [ "$state" == "hardware-installing" ]; then
        # Use crowbar_register mode for claiming the disks, as the OS is
        # already installed
        echo '{ "crowbar_wall": { "registering": true } }' | \
            $chef_client --json-attributes /dev/stdin
    else
        $chef_client
    fi
    # check_machine_role
done

# OK, let looper_chef_client run normally now.
rm /var/run/crowbar/deploying

set_step "transition_crowbar"


# Starting more services
# ----------------------

echo_summary "Starting chef-client"

# Need chef-client daemon now
chkconfig chef-client on
ensure_service_running chef-client

set_step "chef_client_daemon"


# Final sanity checks
# -------------------

echo_summary "Performing post-installation sanity checks"

# Spit out a warning message if we managed to not get an IP address
IP=$($CROWBAR network proposal show default | \
    json_read - attributes.network.networks.admin.ranges.admin.start)
ip addr | grep -q "$IP" || {
    die "eth0 not configured, but should have been."
}

# Run tests -- currently the host will run this.
# 2012-05-24: test is failing, so only run if CROWBAR_RUN_TESTS=true
# (this is distinct from CROWBAR_FROM_GIT, which would add extra repos etc.)
if [ -n "$CROWBAR_RUN_TESTS" ]; then
    /opt/dell/bin/barclamp_test.rb -t || \
        die "Crowbar validation has errors! Please check the logs and correct."
fi

for s in dhcpd apache2 ; do
    if ! service $s status > /dev/null ; then
        die "service $s missing"
    fi
done

set_step "post_sanity_checks"

# We're done!
# -----------

echo_summary "Installation complete!"

touch $crowbar_install_dir/crowbar-installed-ok
rm -f $crowbar_install_dir/crowbar_installing

# activate provisioner repos
curl -X POST http://localhost:3000/utils/repositories/activate_all

kill_spinner

cat <<EOF | pipe_show_and_log


Admin node deployed.

You can now visit the Crowbar web UI at:

    http://$IP/

You should also now be able to PXE-boot a client.  Please refer
to the documentation for the next steps.

Note that to run the crowbar CLI tool, you will need to log out
and log back in again for the correct environment variables to
be set up.
EOF

run_succeeded=hooray
