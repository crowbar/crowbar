#!/bin/bash
# This file is sourced by build_crowbar.sh when you want to build Crowbar 
# using Ubuntu 10.10 as the base OS.  It includes all Ubuntu 10.10 specific
# build routines.

# OS information for the OS we are building crowbar on to.
OS=ubuntu
OS_VERSION=10.10
OS_TOKEN="$OS-$OS_VERSION"
OS_CODENAME=maverick

. "$CROWBAR_DIR/ubuntu-common/build_lib.sh"
