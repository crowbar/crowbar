#!/bin/bash

# Assumes that this is run as root already

FQDN=$1
[[ $FQDN ]] || \
    FQDN=$(grep "cc_hostname_admin:" /etc/openstack/naming.conf | \
           sed "s/^[ 	]*cc_hostname_admin:[ 	]*//")

HOSTNAME=${FQDN%%.*}
[[ $HOSTNAME == $FQDN ]] && HOSTNAME=""
DOMAINNAME=${FQDN#*.}

grep -v "unassigned-hostname" /etc/hosts | \
    grep -v "redundant" /etc/hosts > /tmp/greg.out
mv /tmp/greg.out /etc/hosts
echo "127.0.1.1    $FQDN $HOSTNAME puppet" >> /etc/hosts
chown root.root /etc/hosts

echo "$FQDN" > /etc/hostname
echo "$DOMAINNAME" > /etc/domainname

hostname $FQDN

