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
IP=${HTTP_SERVER%%:*}

echo "deb http://$HTTP_SERVER/ubuntu_dvd maverick main restricted" > /target/etc/apt/sources.list
echo "deb http://$HTTP_SERVER/ubuntu_dvd/extra /" >> /target/etc/apt/sources.list

rsyslog_dir="/target/etc/rsyslog.d"
if [ -d "$rsyslog_dir" ]; then
    if [ ! -f "$rsyslog_dir/10-crowbar-client.conf" ]; then
        echo "*.* @@${IP}" > "$rsyslog_dir/10-crowbar-client.conf"
	if ! wget -O "$rsyslog_dir/00-crowbar-debug.conf" -q http://$HTTP_SERVER/ubuntu_dvd/rsyslog.d/00-crowbar-debug.conf
	then
	    rm -f "$rsyslog_dir/00-crowbar-debug.conf"
	fi
    fi
fi

mkdir -p /target/root/.ssh
chmod 700 /target/root/.ssh
if ! wget -O /target/root/.ssh/authorized_keys.wget -q http://$HTTP_SERVER/ubuntu_dvd/authorized_keys; then
    rm -f /target/root/.ssh/authorized_keys.wget
else
    chmod 644 /target/root/.ssh/authorized_keys
    cat /target/root/.ssh/authorized_keys.wget >>/target/root/.ssh/authorized_keys
    rm -f /target/root/.ssh/authorized_keys.wget
fi

wget -q http://$HTTP_SERVER/ubuntu_dvd/nova_install/crowbar_join.sh -O- > /target/etc/init.d/crowbar_join.sh

sed "s/@@IP/$IP/g" > /target/update_system2.sh <<"EOF"
#!/bin/bash

key_re='crowbar\.install\.key=([^ ]+)'
if [[ $(cat /proc/cmdline) =~ $key_re ]]; then
    export CROWBAR_KEY="${BASH_REMATCH[1]}"
    echo "$CROWBAR_KEY" >/etc/crowbar.install.key
elif [[ -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY="$(cat /etc/crowbar.install.key)"
fi

post_state() {
  local curlargs=(-o "/var/log/$1-$2.json" --connect-timeout 60 -s \
      -L -X POST --data-binary "{ \"name\": \"$1\", \"state\": \"$2\" }" \
      -H "Accept: application/json" -H "Content-Type: application/json" \
      --max-time 240)
  [[ $CROWBAR_KEY ]] && curlargs+=(-u "$CROWBAR_KEY" --digest)
  curl "${curlargs[@]}" "http://@@IP:3000/crowbar/crowbar/1.0/transition/default"
}

HOSTNAME=$(hostname -f)

post_state $HOSTNAME "installing"

sleep 30

post_state $HOSTNAME "installed"

# Wait for DHCP to update - this is mainly for virtual environments or really large deploys
sleep 30

EOF

mount /proc /target/proc
chmod +x /target/etc/init.d/crowbar_join.sh
chmod +x /target/update_system2.sh
chroot /target sed -i "s/__HTTP_SERVER__/$HTTP_SERVER/" /etc/init.d/crowbar_join.sh
chroot /target ln -s /etc/init.d/crowbar_join.sh /etc/rc3.d/S80crowbar
chroot /target ln -s /etc/init.d/crowbar_join.sh /etc/rc5.d/S80crowbar
chroot /target ln -s /etc/init.d/crowbar_join.sh /etc/rc2.d/S80crowbar
chroot /target /update_system2.sh
sync
