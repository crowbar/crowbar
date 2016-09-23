#!/bin/bash
#
# This script starts the upgrade of admin server to the version of system and/or
# Cloud product.
# The script assumes that the repositories are already correctly changed to point
# to the new product versions. After the package upgrade is done, script initiates
# a reboot of the machine.

LOGFILE=/var/log/crowbar/admin-server-upgrade.log
mkdir -p "`dirname "$LOGFILE"`"
exec >>"$LOGFILE" 2>&1

set -x

INSTALLDIR=/var/lib/crowbar/install
RUNDIR=/var/run/crowbar
RUNFILE=$RUNDIR/admin-server-upgrading

cleanup()
{
    echo "cleaning up after interrupt or exit"

    # cleanup upgrading indication so that the action can be restarted
    rm -f $RUNFILE
}

upgrade_admin_server()
{
    mkdir -p $INSTALLDIR
    mkdir -p $RUNDIR

    if [[ -f $RUNFILE ]] ; then
        echo "Exit: Upgrade already running..."
        exit 1
    fi

    if [[ -f $INSTALLDIR/admin-server-upgraded-ok ]] && grep -q "12.2" $INSTALLDIR/admin-server-upgraded-ok ; then
        echo "Exit: Admin server already upgraded"
        exit 0
    fi


    # Signalize that the upgrade is running
    touch $RUNFILE

    trap cleanup INT EXIT

    # we will need the dump for later migrating it into postgresql
    pushd /opt/dell/crowbar_framework
    sudo -u crowbar RAILS_ENV=production bin/rake db:migrate
    sudo -u crowbar RAILS_ENV=production bin/rake db:dump
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
        echo "$ret" > $INSTALLDIR/admin-server-upgrade-failed
        exit $ret
    fi

    # Signalize that the upgrade correctly ended
    echo "12.2" >> $INSTALLDIR/admin-server-upgraded-ok

    # On Cloud7, crowbar-init bootstraps crowbar
    systemctl disable crowbar
    systemctl enable crowbar-init

    # cleanup upgrading indication
    # technically the upgrade is not done yet but it has to be
    # done before the reboot
    rm -f $RUNFILE

    # Reboot after upgrading the system
    reboot
}

# this detaches the process from crowbar and hooks it directly to init
upgrade_admin_server &
