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

lockfile="/tmp/chef-client.lock"
while ! ( set -o noclobber; echo "$$" > "$lockfile") 2> /dev/null; do
    pid=$(cat "$lockfile")
    echo "Failed to aquire lockfile: $lockfile."
    echo "Held by $(readlink "/proc/$pid/exe") ($pid)"
    echo "Process tree for $pid:" 
    pstree -Aupcl "$pid"
    echo
    sleep 1
done
trap 'rm -f "$lockfile"; exit $?' INT TERM EXIT
echo "$$" >"$lockfile"
ret=0
for loglvl in debug debug; do
    chef-client -l "$loglvl" && break
    case $loglvl in
	info) echo "Chef client run failed, will retry with debugging.";;
	debug) echo "Chef client run failed with debug enabled."; ret=1;;
    esac
done
rm -f "$lockfile"
trap - INT TERM EXIT
exit $ret
