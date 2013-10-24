#!/bin/bash
# This is sourced by build_crowbar.sh to enable it to stage Crowbar onto
# OpenSUSE 12.3


# OS information for the OS we are building crowbar on to.
OS=opensuse
OS_VERSION=c12.3
CHROOT_GEMDIR="usr/lib64/ruby/gems/1.9.1/cache"
OSC_IMAGE_DIR=images
OSC_MINSYS="base-meta-x86_64.tar.xz base-x86_64.tar.xz common-base-x86_64.tar.xz"

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="openSUSE-12.3-DVD-x86_64.iso"

OS_REPO_POOL=""

# We do not always want to shrink the generated ISO
# At other times we may want to shrink the ISO otherwise the install will
# fail due to lookingfor packages on the second ISO that we don't have.
SHRINK_ISO=

# The location of OS packages on $ISO
find_cd_pool() ( echo "$IMAGE_DIR/suse/x86_64" )

. "$CROWBAR_DIR/opensuse-common/build_lib.sh"
