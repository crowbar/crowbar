#!/bin/sh
# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

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

IP=`echo $HTTP_SERVER | sed "s/:.*//g"`
HOSTNAME=$(grep host-name /var/lib/dhcp3/dhclient.leases| \
    cut -d'"' -f2 | head -1)

export CROWBAR_KEY=`sed "s/ .*//g" /proc/cmdline | sed "s/.*=//g"`

post_state() {
  echo "{" > /tmp/post_state.$$
  echo "  \"name\": \"$1\"," >> /tmp/post_state.$$
  echo "  \"state\": \"$2\"" >> /tmp/post_state.$$
  echo "}" >> /tmp/post_state.$$
  LD_LIBRARY_PATH=/tmp /tmp/curl -o - --connect-timeout 60 -s -u "$CROWBAR_KEY" \
      --digest --anyauth -L --data-binary @/tmp/post_state.$$ -X POST \
      -H "Accept: application/json" -H "Content-Type: application/json" \
      "http://$IP:3000/crowbar/crowbar/1.0/transition/default" 
  rm /tmp/post_state.$$
}

# get Curl
mkdir -p /tmp
cd /tmp
wget http://$HTTP_SERVER/curl/curl
wget http://$HTTP_SERVER/curl/libcurl.so.4
wget http://$HTTP_SERVER/curl/libidn.so.11
wget http://$HTTP_SERVER/curl/liblber-2.4.so.2
wget http://$HTTP_SERVER/curl/libldap_r-2.4.so.2
wget http://$HTTP_SERVER/curl/libgssapi_krb5.so.2
wget http://$HTTP_SERVER/curl/libssl.so.0.9.8
wget http://$HTTP_SERVER/curl/libcrypto.so.0.9.8
wget http://$HTTP_SERVER/curl/libsasl2.so.2
wget http://$HTTP_SERVER/curl/libgnutls.so.26
wget http://$HTTP_SERVER/curl/libkrb5.so.3
wget http://$HTTP_SERVER/curl/libk5crypto.so.3
wget http://$HTTP_SERVER/curl/libkrb5support.so.0
wget http://$HTTP_SERVER/curl/libkeyutils.so.1
wget http://$HTTP_SERVER/curl/libtasn1.so.3
wget http://$HTTP_SERVER/curl/librt.so.1
wget http://$HTTP_SERVER/curl/libcom_err.so.2 
wget http://$HTTP_SERVER/curl/libgcrypt.so.11
wget http://$HTTP_SERVER/curl/libgpg-error.so.0
chmod +x curl

post_state $HOSTNAME "installing"

