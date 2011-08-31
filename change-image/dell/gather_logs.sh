#!/bin/bash
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

if [[ -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
fi
mkdir -p /tmp/crowbar-logs
tarname="$1"
targetdir="/opt/dell/crowbar_framework/public/logs"
(   flock -s 200
    logdir=$(mktemp -d "/tmp/crowbar-logs/$tarname-XXXXX")
    mkdir -p "$logdir"
    mkdir -p "$targetdir"
    cd "$logdir"
    sshopts=(-q -o 'StrictHostKeyChecking no' 
	-o 'UserKnownHostsFile /dev/null')
    logs=(/var/log /etc)
    for node in $(sudo -H knife node list); do
	mkdir -p "${node%%.*}"
	tarfile="${node%%.*}-${tarname}.tar.gz"
	(   cd "${node%%.*}"
	    [[ $node =~ ^admin ]] && {
		curlargs=(-o /dev/null -D - --connect-timeout 30 --max-time 120)
		[[ $CROWBAR_KEY ]] && curlargs+=(--digest -u "$CROWBAR_KEY")
		for to_get in nodes proposals roles; do
		    curl "${curlargs[@]}" "http://$node:3000/$to_get" || :
		done
		logs+=(/opt/dell/crowbar_framework/db /opt/dell/crowbar_framework/log /install-logs)
	    }
	    sudo ssh "${sshopts[@]}" "${node}" \
	    tar czf "/tmp/$tarfile" "${logs[@]}"
	    sudo scp "${sshopts[@]}" "${node}:/tmp/${tarfile}" "${tarfile}"
	)&
    done &>/dev/null
    wait
    cd ..
    tar cf "$targetdir/crowbar-logs-${tarname}.tar" . &>/dev/null
    rm -rf "$logdir"
) 200>/tmp/crowbar-logs/.lock
