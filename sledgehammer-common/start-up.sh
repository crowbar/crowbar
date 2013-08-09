#!/bin/bash

set -x
shopt -s extglob

function is_suse {
    [ -f /etc/SuSE-release ]
}

DHCPDIR=/var/lib/dhclient
RSYSLOGSERVICE=rsyslog

is_suse && {
 DHCPDIR=/var/lib/dhcp
 RSYSLOGSERVICE=syslog
}

# Figure out where we PXE booted from.
bootif_re='BOOTIF=([^ ]+)'
ip_re='inet ([0-9.]+)/([0-9]+)'
ik_re='crowbar\.install\.key=([^ ]+)'
if [[ $(cat /proc/cmdline) =~ $bootif_re ]]; then
    MAC="${BASH_REMATCH[1]//-/:}"
    MAC="${MAC#*:}"
elif [[ -d /sys/firmware/efi ]]; then
    declare -A boot_entries
    bootent_re='^Boot([0-9]{4})'
    efimac_re='MAC\(([0-9a-f]+)'
    while read line; do
        k="${line%% *}"
        v="${line#* }"
        if [[ $k = BootCurrent:* ]]; then
            current_bootent="${line##BootCurrent: }"
        elif [[ $k =~ $bootent_re ]]; then
            boot_entries["${BASH_REMATCH[1]}"]="$v"
        fi
    done < <(efibootmgr -v)

    if [[ ${boot_entries["$current_bootent"]} =~ $efimac_re ]]; then
        MAC=''
        for o in 0 2 4 6 8 10; do
            MAC+="${BASH_REMATCH[1]:$o:2}:"
        done
        MAC=${MAC%:}
    fi
fi
for nic in /sys/class/net/*; do
    [[ -f $nic/address && -f $nic/type && \
	$(cat "$nic/type") = 1 && \
	$(cat "$nic/address") = $MAC ]] || continue
    BOOTDEV="${nic##*/}"
    break
done
if [[ ! $BOOTDEV ]]; then
    echo "We don't know what the MAC address of our boot NIC was!"
    echo "We will assume we booted off eth0 and hope for the best."
    BOOTDEV=eth0
    MAC=$(cat /sys/class/net/eth0/address)
fi

killall dhclient && sleep 5
# Make sure our PXE interface is up, then fire up DHCP on it.
ip link set "$BOOTDEV" up
dhclient "$BOOTDEV"

if ! [[ $(ip -4 -o addr show dev $BOOTDEV) =~ $ip_re ]]; then
    echo "We did not get an address on $BOOTDEV"
    echo "Things will end badly."
fi
MYIP="${BASH_REMATCH[1]}"

ADMIN_IP=$(grep dhcp-server $DHCPDIR/dhclient*.leases | \
    uniq | cut -d" " -f5 | cut -d";" -f1)
DOMAIN=$(grep "domain-name " $DHCPDIR/dhclient*.leases | \
    uniq | cut -d" " -f5 | cut -d";" -f1 | awk -F\" '{ print $2 }')
HOSTNAME="d${MAC//:/-}.${DOMAIN}"
sed -i -e "s/\(127\.0\.0\.1.*\)/127.0.0.1 $HOSTNAME ${HOSTNAME%%.*} localhost.localdomain localhost/" /etc/hosts
if is_suse; then
    echo "$HOSTNAME" > /etc/HOSTNAME
else
    if [ -f /etc/sysconfig/network ] ; then
      sed -i -e "s/HOSTNAME=.*/HOSTNAME=${HOSTNAME}/" /etc/sysconfig/network
    fi
    echo "${HOSTNAME#*.}" >/etc/domainname
fi
hostname "$HOSTNAME"
HOSTNAME_MAC="$HOSTNAME"

[[ $(cat /proc/cmdline) =~ $ik_re ]] && \
    export CROWBAR_KEY="${BASH_REMATCH[1]}"

# enable remote logging to our admin node.
echo "# Sledgehammer added to log to the admin node" >> /etc/rsyslog.conf
echo "*.* @@${ADMIN_IP}" >> /etc/rsyslog.conf
service $RSYSLOGSERVICE restart

# Setup common dirs
exports=$(showmount -e $ADMIN_IP --no-headers | awk {'print $1'})
for d in /updates /install-logs /var/log/crowbar/sledgehammer; do
    for e in $exports; do
        if [ "$e" == "$d" ]; then
            mkdir -p $d
            mount -t nfs $ADMIN_IP:$d $d
        fi
    done
done

export MAC BOOTDEV ADMIN_IP DOMAIN HOSTNAME HOSTNAME_MAC MYIP

cd /updates
cp /updates/control.sh /tmp
/tmp/control.sh
