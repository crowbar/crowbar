#!/bin/bash

set -x

# Figure out where we PXE booted from.
bootif_re='BOOTIF=([^ ]+)'
ip_re='inet ([0-9.]+)/([0-9]+)'
ik_re='crowbar\.install\.key=([^ ]+)'
if [[ $(cat /proc/cmdline) =~ $bootif_re ]]; then
    MAC="${BASH_REMATCH[1]//-/:}"
    MAC="${MAC#*:}"
    for nic in /sys/class/net/*; do
	[[ -f $nic/address && -f $nic/type && \
	    $(cat "$nic/type") = 1 && \
	    $(cat "$nic/address") = $MAC ]] || continue
	BOOTDEV="${nic##*/}"
	break
    done
fi
if [[ ! $BOOTDEV ]]; then
    echo "We don't know what the MAC address of our boot NIC was!"
    echo "We will assume we booted off eth0 and hope for the best."
    BOOTDEV=eth0
    MAC=$(cat /sys/class/net/eth0/address)
fi

# Make sure our PXE interface is up, then fire up DHCP on it.
ip link set "$BOOTDEV" up
dhclient "$BOOTDEV"

if ! [[ $(ip -4 -o addr show dev $BOOTDEV) =~ $ip_re ]]; then
    echo "We did not get an address on $BOOTDEV"
    echo "Things will end badly."
fi
MYIP="${BASH_REMATCH[1]}"

ADMIN_IP=$(grep dhcp-server /var/lib/dhclient/dhclient*.leases | \
    uniq | cut -d" " -f5 | cut -d";" -f1)
DOMAIN=$(grep "domain-name " /var/lib/dhclient/dhclient*.leases | \
    uniq | cut -d" " -f5 | cut -d";" -f1 | awk -F\" '{ print $2 }')
HOSTNAME_MAC="h${MAC//:/-}.${DOMAIN}"

[[ $(cat /proc/cmdline) =~ $ik_re ]] && \
    export CROWBAR_KEY="${BASH_REMATCH[1]}"
HOSTNAME=$(hostname)

[[ $HOSTNAME = localhost.localdomain ]] &&  {
    hostname $HOSTNAME_MAC
    HOSTNAME=${HOSTNAME_MAC}
}

# enable remote logging to our admin node.
echo "# Sledgehammer added to log to the admin node" >> /etc/rsyslog.conf
echo "*.* @@${ADMIN_IP}" >> /etc/rsyslog.conf
service rsyslog restart

# Setup common dirs
for d in updates install-logs; do
    mkdir -p /$d
    mount -t nfs $ADMIN_IP:/$d /$d
done

export MAC BOOTDEV ADMIN_IP DOMAIN HOSTNAME_MAC HOSTNAME MYIP

cd /updates
./control.sh
