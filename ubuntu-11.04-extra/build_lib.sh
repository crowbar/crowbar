#!/bin/bash
# This file is sourced by build_crowbar.sh when you want to build Crowbar 
# using Ubuntu 11.04 as the base OS.  It includes all Ubuntu 11.04 specific
# build routines.

# OS information for the OS we are building crowbar on to.
OS=ubuntu
OS_VERSION=11.04
OS_TOKEN="$OS-$OS_VERSION"
OS_CODENAME=natty

. "$CROWBAR_DIR/ubuntu-common/build_lib.sh"