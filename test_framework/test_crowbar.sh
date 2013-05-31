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

# We use bash4 specific functionality (hash tables), and cgroups to
# make sure we clean up everything when we exit a test run.
# You will need a fairly recent Linux distro to run this test --
# RHEL/CentOS 5 will not work without some significant rework.

readonly currdir="$PWD"

# Location of the Crowbar checkout we are building from.
[[ $CROWBAR_DIR ]] || CROWBAR_DIR="${0%/*}/.."
[[ $CROWBAR_DIR = /* ]] || CROWBAR_DIR="$currdir/$CROWBAR_DIR"
[[ -f $CROWBAR_DIR/build_crowbar.sh && -d $CROWBAR_DIR/.git ]] || \
    die "$CROWBAR_DIR is not a git checkout of Crowbar!"
export CROWBAR_DIR

. "$CROWBAR_DIR/build_lib.sh"
. "$CROWBAR_DIR/test_lib.sh"

do_help() {
    cat <<EOF
$0: Crowbar test framework.
I smoketest Crowbar to ensure we have certian minimal functionality.

Arguments:
   cleanup: Will try to clean up from any previous runs that
            exited abnormally.
   develop-mode: Causes the framework to pause instead of automatically
                 exiting whenever a test fails, a timeout is exceeded, or
                 the test would otherwise exit sucessfully.
   pause: Causes the framework unconditionally pause after all the tests
          have finished.
   pause-after-admin: Causes the framework to pause after the admin node
                      has been deployed.
   admin-only: Only deploy an admin node -- do not spin up compute nodes.
   manual-deploy: By default, the framework will automatically launch the
                  crowbar install once the admin node is up using a default
                  name of admin.smoke.test.  Passing manual-deploy causes
                  the framework to not do that.
   use-iso <path-to-iso>: This tells the framework what iso to use to try
                          to use to deploy Crowbar from.
   <barclamp name>: Deploy and run any applicable smoketests for <barclamp>.
   help: Display this help

VM Specifications:
   By default, the smoketest framework will try and deploy a 5 node cluster.

   The admin node will have 4 CPUs, 4 gigs of RAM, 1 hard drive with
   15 gigs of disk space, and 2x E1000 nics.

   The compute nodes will have 2 CPUs, 2 gigs of RAM, i hard drive with
   10 gigs of space for the OS, 2 hard drives with a gig of space,
   and 2x E1000 nics.

Networking:
   The smoketest framework creates 2 bridges, and attaches one nic from each
   VM to each bridge.  The primary bridge is given an IP address of
   192.168.124.1, and the framework assumes that it can contact the admin node
   at 192.168.124.10.

   The framework will also inject the public key of the user running the
   smoketest into all of the deployed nodes to enable passwordless SSH access
   to all the public interfaces on all the deployed nodes.

Logging and Display:
   By default, the smoketest framework will arrange for all the VMs to log
   to a serial capture, which is saved in $CROWBAR_DIR/testing.  This is also
   where error output from the scripts are captured, the VM images and state
   information is saved, etc.  The framework also runs a screen session to
   display various important logs, provide a shell running in the same
   environment as the framework, and (if the DISPLAY variable is not present
   in the environment) to provide access to the consoles of the vms.
   If DISPLAY is set, the VMs will run using an SDL display.

For more information, please see $CROWBAR_DIR/test_framwwork/README.testing.
EOF
}

[[ -f $HOME/.ssh/id_rsa.pub ]] || { mkdir -p "$HOME/.ssh"; ssh-keygen -q -f "$HOME/.ssh/id_rsa" -t rsa -N '' ; }

case $1 in
    cleanup) shift;
            CGROUP_DIR=$(sudo "$SMOKETEST_DIR/make_cgroups.sh" $$ crowbar-test) || \
                die "Could not mount cgroup filesystem!"
            for smoketest_dir in "$CROWBAR_DIR/testing/"*; do
                [[ -d $smoketest_dir && $smoketest_dir != */cli ]] || continue
                smoketest_cleanup
            done
            unset smoketest_dir
            smoketest_cleanup
            for l in "$SMOKETEST_LOCK" "$SMOKETEST_KVM_LOCK" \
                "$SMOKETEST_CLEANUP_LOCK"; do
                rm -f "$l"
            done;;
    ''|help) do_help;;
    run-test) shift; run_test "$@" ;;
    *) run_test "$@" ;;
esac
