#!/bin/bash

# OS information for the OS we are building crowbar on to.
OS=opensuse
OS_VERSION=12.3
OS_TOKEN="$OS-$OS_VERSION"

# This replaces the do_crowbar_build function that is defined in $CROWBAR_DIR/build_lib.sh which
#   gets sourced into the $CROWBAR_DIR/build_crowbar.sh script.

do_crowbar_build() {

	[[ -x $CROWBAR_DIR/$OS_TOKEN-extra/kiwi ]] && cd $CROWBAR_DIR/$OS_TOKEN-extra/kiwi
	[[ -f create_appliance.sh ]] && sudo ./create_appliance.sh
}
