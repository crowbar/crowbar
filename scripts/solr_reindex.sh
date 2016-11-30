#!/bin/bash
#
# This script rebuilds the chef solr search index.

if [ ! -f /var/lib/crowbar/install/crowbar-installed-ok ]; then
  echo "Crowbar is not installed, skipping..."
  exit 0
fi

if ! grep -q "/dev/shm/solr_data" /etc/chef/solr.rb; then
  echo "Solr tmpfs isn't used, skipping..."
  exit 0
fi

if [ ! -f /root/.chef/knife.rb ]; then
  echo "Can't find knife config at /root/.chef/knife.rb"
  exit 1
fi

/usr/bin/knife index rebuild -y -c /root/.chef/knife.rb
