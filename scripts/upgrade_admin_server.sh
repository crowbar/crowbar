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

UPGRADEDIR=/var/lib/crowbar/upgrade
RUNDIR=/var/run/crowbar
RUNFILE=$RUNDIR/admin-server-upgrading

cleanup()
{
    echo "cleaning up after interrupt or exit"

    # cleanup upgrading indication so that the action can be restarted
    rm -f $RUNFILE
}

report_failure()
{
    local ret=$1
    local errmsg=$2

    echo "$ret" > $UPGRADEDIR/admin-server-upgrade-failed
    echo $errmsg
    # The status of the upgrade needs to be set in case of an error
    # so we can just use the upgrade_status library to set the status to "failed"
    # otherwise the status of the admin_upgrade would stay at "running" which would prevent
    # continuation of the upgrade even if the admin upgrade got fixed manually
    ruby -e "
        require 'logger'
        require '/opt/dell/crowbar_framework/lib/crowbar/upgrade_status'
        ::Crowbar::UpgradeStatus.new(Logger.new(STDOUT)).end_step(
            false,
            admin_upgrade: {
                data: '$errmsg',
                help: 'Failed to upgrade admin server. Refer to the error message in the response.'
            }
    )"
    exit $ret
}


upgrade_admin_server()
{
    mkdir -p $UPGRADEDIR
    mkdir -p $RUNDIR

    if [[ -f $RUNFILE ]] ; then
        echo "Exit: Upgrade already running..."
        exit 1
    fi

    if [[ -f $UPGRADEDIR/admin-server-upgraded-ok ]] && grep -q "12.2" $UPGRADEDIR/admin-server-upgraded-ok ; then
        echo "Exit: Admin server already upgraded"
        exit 0
    fi

    # Remove possible failed file before starting (again)
    rm -f $UPGRADEDIR/admin-server-upgrade-failed

    # Signalize that the upgrade is running
    touch $RUNFILE

    trap cleanup INT EXIT

    # we will need the dump for later migrating it into postgresql
    pushd /opt/dell/crowbar_framework
    sudo -u crowbar RAILS_ENV=production bin/rake db:migrate
    cp -a db/schema.rb /var/lib/crowbar/upgrade
    sudo -u crowbar RAILS_ENV=production bin/rake db:dump
    cp -a db/data.yml /var/lib/crowbar/upgrade
    popd

    ### Chef-client could lockj zypper and break upgrade
    rcchef-client stop

    # Update the OS values for admin node
    knife exec -E "n = nodes.find(:roles => 'provisioner-server').first
n.target_platform = 'suse-12.2'
n.provisioner.default_os = 'suse-12.2'
n.save"

    ret=$?
    if [ $ret != 0 ]; then
        report_failure $ret "Setting the platform to suse-12.2 has failed."
    fi

    # Upgrade the distribution non-interactively
    zypper --no-color --releasever 12.2 ref -f
    zypper --no-color --non-interactive dist-upgrade -l --recommends --replacefiles
    ret=$?
    if [ $ret != 0 ]; then
        # In the failed case, crowbar should tell user to check zypper logs,
        # fix the errors and continue admin server manually
        report_failure $ret "zypper dist-upgrade has failed with $ret, check zypper logs"
    fi

    # Signalize that the upgrade correctly ended
    echo "12.2" >> $UPGRADEDIR/admin-server-upgraded-ok

    # On Cloud7, crowbar-init bootstraps crowbar
    systemctl disable crowbar
    systemctl enable crowbar-init

    # remove old (chef-created) crowbar systemd unit file, the file
    # is part of the package now and installed in /usr/lib/systemd/
    rm -f /etc/systemd/system/crowbar.service
    systemctl daemon-reload

    # cleanup upgrading indication
    # technically the upgrade is not done yet but it has to be
    # done before the reboot
    rm -f $RUNFILE

    # Reboot after upgrading the system
    reboot
}

# this detaches the process from crowbar and hooks it directly to init
upgrade_admin_server &
