#!/bin/bash
#
# Script: clean_rpms.sh
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

declare -a pool

# first, build a list of all the rpms currently in the pool.
while read pool_rpm; do
    pool+=("$pool_rpm")
done <(find ../pool/* -type f)

# some matchers to use
rpm_exact_match() { [[ ${1##*/} = ${2##*/} ]]; }
rpm_subpart_match() { 
    local _b1=${1##*/} _b2=${2##*/}
    [[ ${_b1%%_*} = ${_b2%%_*} ]]
}

# $1 = rpm to check
# $2 = matching function
rpm_in_pool() {
    for ((idx = 0;idx <${#pool[@]}; idx++)); do
	$2 "$1" "${pool[idx]}" && return 0
    done
    return 1
}

find rpms | while read rpm
do
    if rpm_in_pool "$rpm" rpm_exact_match; then
	echo "# $RPMFILE already in pool.  Removing"
	rm $rpm
    elif rpm_in_pool "$rpm" rpm_subpart_match; then
	echo ""
	echo "# $RPMFILE  ==  `basename $NEW_FILE`"
	echo "rm $rpm"
    fi
done
