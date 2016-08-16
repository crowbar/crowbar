#!/bin/bash
#
# This script starts the upgrade of admin server to the version of system and/or
# Cloud product.
# The script assumes that the repositories are already correctly changed to point
# to the new product versions. After the package upgrade is done, script initiates
# a reboot of the machine.


upgrade_admin_server()
{   
    # Upgrade the distribution non-interactively
    zypper --non-interactive dist-upgrade -l --recommends --replacefiles

    # Signalize that the upgrade correctly ended
    touch /var/lib/crowbar/install/admin-server-upgraded-ok

    # On Cloud7, the service that should started is called crowbar-init
    systemctl disable crowbar
    systemctl enable crowbar-init

    # Reboot after upgrading the system
    reboot
}

# this detaches the process from crowbar and hooks it directly to init
upgrade_admin_server &
