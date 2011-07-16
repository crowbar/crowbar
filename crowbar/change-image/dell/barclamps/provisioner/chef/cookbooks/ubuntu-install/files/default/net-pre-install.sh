#!/bin/sh
# This script is called by the net_ubuntu.seed file during
# ubuntu install.  It handles doing the manual sets needed to layout the
# filesystem the rest of the way before running the common_install.sh.
#
# The intent is that once the system is finished installing, the customer
# will edit the config.json file and run install.sh to complete installation
# and initial configuration.
#

#
# We are a net install. 
#

HTTP_SERVER=$1

IP=${HTTP_SERVER%%:*}
HOSTNAME=$(grep host-name /var/lib/dhcp3/dhclient.leases| \
    cut -d'"' -f2 | head -1)

export CROWBAR_KEY="$(grep -o 'crowbar\.install\.key=[^ ]*' /proc/cmdline)"
if [ -z "$CROWBAR_KEY" ]; then
 echo "Cannot find install key, this will not end well."
fi

post_state() {
  echo "{" > /tmp/post_state.$$
  echo "  \"name\": \"$1\"," >> /tmp/post_state.$$
  echo "  \"state\": \"$2\"" >> /tmp/post_state.$$
  echo "}" >> /tmp/post_state.$$
  curl -o - --connect-timeout 60 -s -u "$CROWBAR_KEY" \
      --digest -L --data-binary @/tmp/post_state.$$ -X POST \
      -H "Accept: application/json" -H "Content-Type: application/json" \
      "http://$IP:3000/crowbar/crowbar/1.0/transition/default" 
  rm /tmp/post_state.$$
}

post_state $HOSTNAME "installing"
