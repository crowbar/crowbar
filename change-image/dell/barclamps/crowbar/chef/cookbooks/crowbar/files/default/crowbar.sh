#
# Default path variables.
#

# Make sure /opt/dell/bin is in the root path
if ! echo ${PATH} | /bin/grep -q /opt/dell/bin ; then
        if [ `/usr/bin/id -u` = 0 ] ; then
                PATH=${PATH}:/opt/dell/bin
        fi
fi

if [ -f /etc/crowbar.install.key ] ; then
    export CROWBAR_KEY=`cat /etc/crowbar.install.key`
fi

