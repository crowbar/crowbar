#!/bin/bash
#
# This script starts the upgrade of admin server to the version of system and/or
# Cloud product.
# The script assumes that the repositories are already correctly changed to point
# to the new product versions. After the package upgrade is done, script initiates
# a reboot of the machine.

LOGFILE=/var/log/crowbar/admin-server-upgrade.log
mkdir -p "`dirname "$LOGFILE"`"

# Copy stdout to fd 3
exec 3>&1
# Create fd 4 for logfile
exec 4>> "$LOGFILE"
# Set fd 1 and 2 to logfile
exec 1>&4 2>&1

set -x

upgrade_admin_server()
{
    # we will need the dump for later migrating it into postgresql
    pushd /opt/dell/crowbar_framework
    export RAILS_ENV=production
    bin/rake db:migrate
    bin/rake db:dump
    popd

    ### Chef-client could lockj zypper and break upgrade
    rcchef-client stop

    # Upgrade the distribution non-interactively
    zypper --no-color --releasever 12.2 ref -f
    zypper --no-color --non-interactive dist-upgrade -l --recommends --replacefiles
    ret=$?
    if [ $ret != 0 ]; then
        # In the failed case, crowbar should tell user to check zypper logs,
        # fix the errors and continue admin server manually
        echo "zypper dist-upgrade has failed with $ret, check zypper logs"
        echo "$ret" > /var/lib/crowbar/install/admin-server-upgrade-failed
        return
    fi

    # Signalize that the upgrade correctly ended
    touch /var/lib/crowbar/install/admin-server-upgraded-ok

    # On Cloud7, crowbar-init bootstraps crowbar
    systemctl disable crowbar
    systemctl enable crowbar-init

    # Reboot after upgrading the system
    reboot
}

# this detaches the process from crowbar and hooks it directly to init
upgrade_admin_server &
