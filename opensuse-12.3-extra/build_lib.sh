#!/bin/bash
# Copyright 2013, Dell
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
# Author: John H Terpstra
#
#
# This script is sourced into $CROWBAR_DIR/build_crowbar.sh and is generally called
# from the $CROW?BAR_DIR/dev tool.
#   Example Usage:  ./dev build --os opensuse-12.3
#
# NOTE: Do not call this script directly as that may have undesirable side-effects.
#
# OS information for the OS we are building crowbar on to.
OS=opensuse
OS_VERSION=12.3
OS_TOKEN="$OS-$OS_VERSION"
SMOKETEST_ISO="$ISO_DEST/crowbar-$OS_TOKEN-$(crowbar_version).iso"

# This replaces the do_crowbar_build function that is defined in $CROWBAR_DIR/build_lib.sh which
#   gets sourced into the $CROWBAR_DIR/build_crowbar.sh script.

do_safe_kiwi_build() (
    # Change to the scratch pad directory
    cd $CACHE_DIR/$OS_TOKEN-extra/kiwi

    # Go ahead an create the ISO (appliance) if we can.
    [[ -f create_appliance.sh ]] && ./create_appliance.sh  || die "KIWI create_appliance problem!"

    # If the build succedded there will be an ISO file - now copy it to the $ISO_DEST directory
    if [[ $(echo $CACHE_DIR/$OS_TOKEN-extra/kiwi/image/*.iso) ]]; then
        NEWISO="$(echo $CACHE_DIR/$OS_TOKEN-extra/kiwi/image/*.iso)"
        log "Moving $NEWISO to $SMOKETEST_ISO"
        cp $NEWISO $SMOKETEST_ISO || die "ISO Build Failed!"
	sudo rm $NEWISO && log "Deleting $NEWISO to get it out of the way."
        fi

)

test_kiwi_iso() {
    run_test "$@" || \
        die "$(date '+%F %T %z'): Smoketest of $SMOKETEST_ISO failed."
}

do_crowbar_build() {

    # Verify that the KIWI config files are in place.
    [[ -x $CROWBAR_DIR/$OS_TOKEN-extra/kiwi ]] && cd $CROWBAR_DIR/$OS_TOKEN-extra/kiwi || \
	die "Can't do KIWI Build - Check KIWI config."

    # Remove all remnants of previous builds - this step makes sure we do not treat older builds as current.
    [[ -d $CROWBAR_DIR/$OS_TOKEN-extra/kiwi/image/build ]] && \
	sudo rm -rf $CROWBAR_DIR/$OS_TOKEN-extra/kiwi/image/build/.
    [[ ! -e $CROWBAR_DIR/$OS_TOKEN-extra/kiwi/image/build ]] || \
	mkdir -p $CROWBAR_DIR/$OS_TOKEN-extra/kiwi/image/build && \
	log "$CROWBAR_DIR/$OS_TOKEN-extra/kiwi/image/build - OK"

    # We want to keep out code tree clean so copy the kiwi build files to a safe working area
    #  Our safe scratch-pad is in the $CACHE_DIR
    [[ -d $CACHE_DIR/$OS_TOKEN-extra ]] || ( mkdir -p $CACHE_DIR/$OS_TOKEN-extra || \
	die "Something went wrong with the use of -d $CACHE_DIR/$OS_TOKEN-extra!" )

    # Copy all our build files into $CACHE_DIR/$OS_TOKEN-extra - make it clean!
    ( cd $CROWBAR_DIR/$OS_TOKEN-extra && sudo rsync -av --delete . $CACHE_DIR/$OS_TOKEN-extra/. )

    do_safe_kiwi_build
    echo "$(date '+%F %T %z'): Image at $SMOKETEST_ISO"
    if [[ $NEED_TEST = true ]]; then
        echo "$(date '+%F %T %z'): Testing new iso"
        test_kiwi_iso "${test_params[@]}" && \
            echo "$(date '+%F %T %z'): Test passed" || \
            die "Test failed."
    fi
    echo "$(date '+%F %T %z'): Finished."

}
