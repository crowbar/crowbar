#!/bin/bash
# This file is sourced by build_crowbar.sh when you want to build Crowbar 
# using Ubuntu 10.10 as the base OS.  It includes all Ubuntu 10.10 specific
# build routines.

# OS information for the OS we are building crowbar on to.
OS=ubuntu
OS_VERSION=12.04
OS_TOKEN="$OS-$OS_VERSION"
OS_CODENAME=precise
ISO=ubuntu-12.04-server-amd64.iso
# uncomment to use the daily beta build, if you have it...
#ISO=precise-server-amd64.iso

. "$CROWBAR_DIR/ubuntu-common/build_lib.sh"
