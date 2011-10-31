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
set -e

cgroups="$HOME/cgroups"
mount=true
if [[ -f /sys/fs/cgroup/cpu/tasks ]]; then
    cgroups=/sys/fs/cgroup/cpu
else
    {
	while read dev loc type opts rest; do
	    [[ $type = cgroup ]] || continue
	    cgroups=$loc
	    mount=false
	    break
	done < /proc/self/mounts
	if [[ $mount = true ]]; then
	    mkdir -p "$cgroups"
	    mount -t cgroup -o cpu xxx "$cgroups"
	fi
	
    } >&2
fi
mkdir -p "$cgroups/$2"
echo "$cgroups/$2"
echo "$1" >"$cgroups/$2/tasks"

