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
[[ $CROWBAR_DIR ]] || CROWBAR_DIR="${0%/*}"
[[ $CROWBAR_DIR = /* ]] || CROWBAR_DIR="$currdir/$CROWBAR_DIR"
[[ -f $CROWBAR_DIR/build_crowbar.sh && -d $CROWBAR_DIR/.git ]] || \
    die "$CROWBAR_DIR is not a git checkout of Crowbar!" 
export CROWBAR_DIR

. "$CROWBAR_DIR/build_lib.sh"
. "$CROWBAR_DIR/test_lib.sh"

SMOKETEST_BRIDGES=(deploy-br)
NICS_PER_BRIDGE=1


[[ -f $1 && $1 = *Crowbar_Installer.vmdk ]] || \
    die "$1 is not a crowbar installer disk image!"
[[ -f $2 && $2 = *crowbar-*.iso ]] || \
    die "$2 is not a croebar ISO!"
[[ -d /sys/class/net/$3 ]] || \
    die "$3 is not an interface to let the installer serve PXE boots!"
PHYSICAL_INTERFACES=("$3,deploy-br")
make_virt_net

kvm -drive "file=$1,if=ide,media=disk,snapshot=on" \
    -cdrom "$2" \
    -net "nic,macaddr=52:54:00:00:00:8f,model=e1000" \
    -net "tap,ifname=admin-0-br,script=no,downscript=no" \
    -m 512 -snapshot
kill_virt_net