#!/bin/bash

set -x
shopt -s extglob

# File /etc/SuSE-release is deprecated and will be removed
# in a future service pack or release. Therefore we would
# like to use /etc/os-release about details release,
# but for SLES 11 SP3 /etc/os-release doesn't exist.
function is_suse() {
    # This check will work on SLE 11 SP3 and SLE 12
    [ -f /etc/SuSE-release ] && return

    # This will work only on SLE 12 and above, after
    # /etc/SuSE-release will be deprecated
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        [ "$NAME" == "SLES" ] && return
    fi

    return 1
}

function suse_ver() {
    local ver=$1
    local suse_ver=0

    if [ -f /etc/os-release ]; then
        . /etc/os-release
        [ "$NAME" == "SLES" ] && suse_ver=${VERSION%-SP[0-9]}
    elif [ -f /etc/SuSE-release ]; then
        suse_ver=`cat /etc/SuSE-release  | awk '/VERSION/ {print $3}'`
    fi

    [ "$suse_ver" -eq "$ver" ]
}

if suse_ver 11; then
    DHCPDIR=/var/lib/dhcp
    RSYSLOGSERVICE=syslog
else
    DHCPDIR=/var/lib/dhclient
    RSYSLOGSERVICE=rsyslog
fi

# Really only for debug mode
passwd_re='DISCOVERY_ROOT_PASSWORD=([^ ]+)'
state_re='crowbar\.state=([^ ]+)'
if [[ $(cat /proc/cmdline) =~ $state_re ]] && [ ${BASH_REMATCH[1]} == discovery ]; then
    if [[ $(cat /proc/cmdline) =~ $passwd_re ]]; then
        ROOT_PASSWORD="${BASH_REMATCH[1]}"
        echo "root:$ROOT_PASSWORD" | chpasswd
    fi
fi

# if the admin ip was passed on the cmdline grab it
admin_ip_re='adminip=([^ ]+)'
if [[ $(cat /proc/cmdline) =~ $admin_ip_re ]]; then
    ADMIN_IP="${BASH_REMATCH[1]}"
fi

# Figure out where we PXE booted from.
MAC=
bootif_re='BOOTIF=([^ ]+)'
ip_re='inet ([0-9.]+)/([0-9]+)'
ip6_re='inet6 ([0-9a-f:]+)/([0-9]+)'
if [[ $(cat /proc/cmdline) =~ $bootif_re ]]; then
    MAC="${BASH_REMATCH[1]//-/:}"
    MAC="${MAC#*:}"
elif [[ -d /sys/firmware/efi ]]; then
    declare -A boot_entries
    bootent_re='^Boot([0-9a-fA-F]{4})'
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

if ! suse_ver 12; then
    killall dhclient && sleep 5
    # Make sure our PXE interface is up, then fire up DHCP on it.
    ip link set "$BOOTDEV" up
    dhclient "$BOOTDEV"
else
    if [ "$BOOTDEV" != "eth0" ]; then
        mv /etc/sysconfig/network/ifcfg-eth0 /etc/sysconfig/network/ifcfg-$BOOTDEV
        wicked ifup --timeout ${WAIT_FOR_INTERFACES:-120} $BOOTDEV
    fi
fi

IP_VERSION=4
if ! [[ $(ip -4 -o addr show dev $BOOTDEV) =~ $ip_re ]]; then
    if [[ $(ip -6 -o addr show dev $BOOTDEV) =~ $ip6_re ]]; then
        IP_VERSION=6
    else
        echo "We did not get an address on $BOOTDEV"
        echo "Things will end badly."
    fi
fi
MYIP="${BASH_REMATCH[1]}"

if suse_ver 12; then
    [ -f /etc/sysconfig/network/config ] && source /etc/sysconfig/network/config
    WAIT_FOR_INTERFACES=${WAIT_FOR_INTERFACES:-120}
    /usr/lib/wicked/bin/wickedd-dhcp$IP_VERSION --test --test-output /tmp/wicked-dhcp-$BOOTDEV --test-timeout $WAIT_FOR_INTERFACES $BOOTDEV
    source /tmp/wicked-dhcp-$BOOTDEV
    if [ -z "$ADMIN_IP" ]; then
        ADMIN_IP=$SERVERID
    fi
    if [ -z "$DNSDOMAIN" ]; then
        # DNSSEARCH can potentually be a comma seperated list of search domains
        # so below we're making sure we only grab the first.
        DOMAIN=${DNSSEARCH%%,*}
    else
        DOMAIN=$DNSDOMAIN
    fi
else
    if [ -z "$ADMIN_IP" ]; then
        ADMIN_IP=$(grep dhcp-server $DHCPDIR/dhclient*.leases | \
            uniq | cut -d" " -f5 | cut -d";" -f1)
    fi
    DOMAIN=$(grep "domain-name " $DHCPDIR/dhclient*.leases | \
        uniq | cut -d" " -f5 | cut -d";" -f1 | awk -F\" '{ print $2 }')
fi
if [ -n "$DOMAIN" ]; then
    HOSTNAME="d${MAC//:/-}.${DOMAIN}"
else
    HOSTNAME="d${MAC//:/-}"
fi

sed -i -e "s/\(127\.0\.0\.1.*\)/127.0.0.1 $HOSTNAME ${HOSTNAME%%.*} localhost.localdomain localhost/" /etc/hosts
if (( $IP_VERSION == 6 )); then
    sed -i -e "s/\(\:\:1.*\)/::1 $HOSTNAME ${HOSTNAME%%.*} localhost.localdomain localhost ipv6-localhost ipv6-loopback/" /etc/hosts
fi
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

ik_re='crowbar\.install\.key=([^ ]+)'
[[ $(cat /proc/cmdline) =~ $ik_re ]] && \
    export CROWBAR_KEY="${BASH_REMATCH[1]}"

# enable remote logging to our admin node.
echo "# Sledgehammer added to log to the admin node" >> /etc/rsyslog.conf
echo "*.* @@${ADMIN_IP}" >> /etc/rsyslog.conf
service $RSYSLOGSERVICE restart

# Sometimes at this point network is not up yet, wait for it
ping="ping"
if (( $IP_VERSION == 6 )); then
    ping="ping6"
fi
n=60
echo "Waiting for admin server ($ADMIN_IP) to be reachable; will wait up to $n seconds..."
while (( $n > 0 )) && ! $ping -q -c 1 -w 1 $ADMIN_IP > /dev/null ; do
    sleep 1
    let n--
done
if (( $n == 0 )); then
    echo "Admin server ($ADMIN_IP) not reachable."
    echo "Things will end badly."
fi

# showmount needs a running rpcbind service
service rpcbind start

# Setup common dirs based on what the Crowbar admin server is sharing
exports=$(showmount -e $ADMIN_IP --no-headers | cut -f1 -d " ")
for d in $exports; do
    mkdir -p $d
    if (( $IP_VERSION == 6 )); then
        mount -t nfs [$ADMIN_IP]:$d $d
    else
        mount -t nfs $ADMIN_IP:$d $d
    fi
done

export MAC BOOTDEV ADMIN_IP ADMIN_IP_WRAPPED DOMAIN HOSTNAME HOSTNAME_MAC MYIP IP_VERSION

cd /updates
cp /updates/control.sh /tmp
/tmp/control.sh
