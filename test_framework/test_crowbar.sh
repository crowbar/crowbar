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

# This script expects to be able to run certian commands as root.
# Either run it as a user who can sudo to root, or give the user
# you are running it as the following sudo rights:
# crowbar-tester ALL = NOPASSWD: /bin/mount, /bin/umount, /bin/ip, /usr/sbin/brctl, /home/crowbar-tester/test_framework/make-cgroups.sh

# We expect to live in $HOME/test_framework.
# We use bash4 specific functionality (hash tables), and cgroups to
# make sure we clean up everything when we exit a test run.
# You will need a fairly recent Linux distro to run this test -- 
# RHEL/CentOS 5 will not work without some significant rework.

# Include common build and smoketest functionality
if ! [[ $CROWBAR_DIR && -d $CROWBAR_DIR ]]; then
    echo "Cannot find \$CROWBAR_DIR, we cannot run tests."
    exit 1
fi

. "$CROWBAR_DIR/build_lib.sh"
. "$CROWBAR_DIR/test_lib.sh"

do_help() {
    cat <<EOF
$0: Crowbar test framework.
I smoketest Crowbar to ensure we have certian minimal functionality.

If you run me without options, I will run a smoketest 
If you run me with run-failed <machine list>, I will try to run
vms from the last failed run.

Read my comments to learn how to tweak me.
EOF
}

[[ -f $HOME/.ssh/known_hosts ]] && rm -f "$HOME/.ssh/known_hosts" || :
[[ -f $HOME/.ssh/id_rsa.pub ]] || { mkdir -p "$HOME/.ssh"; ssh-keygen -q -f "$HOME/.ssh/id_rsa" -t rsa -N '' ; }

[[ -f $HOME/testing/cli ]] || mkdir -p "$HOME/testing/cli"
export PATH="$HOME/testing/cli:$PATH"

CGROUP_DIR=$(sudo "$SMOKETEST_DIR/make_cgroups.sh" $$ crowbar-test) || \
    die "Could not mount cgroup filesystem!"


case $1 in
    cleanup) shift; 
	for l in "$SMOKETEST_LOCK" "$SMOKETEST_KVM_LOCK" "$SMOKETEST_CLEANUP_LOCK"; do
	    rm -f "$l"
	done
	smoketest_cleanup;;
    ''|help) do_help;;
    *) run_test "$@" ;;
esac
