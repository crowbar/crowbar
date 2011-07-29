#!/bin/bash
#
# Script: instal-chef.sh
#
# Copyright (c) 2011 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Assumes that this is run as root already
#

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
echo "127.0.1.1 $FQDN $HOSTNAME puppet" >> /etc/hosts
chown root.root /etc/hosts

echo "$FQDN" > /etc/hostname
echo "$DOMAINNAME" > /etc/domainname

hostname $FQDN

