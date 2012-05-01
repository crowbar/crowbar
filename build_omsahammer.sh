#!/bin/bash
#
# Build a sledgehammer image for Crowbar and put it in the build cache.

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
# Author: VictorLowther

# We always use the C language and locale
export LANG="C"
export LC_ALL="C"

GEM_RE='([^0-9].*)-([0-9].*)'

readonly currdir="$PWD"
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin"

if ! which cpio &>/dev/null; then
    die "Cannot find cpio, we cannot proceed."
fi

if ! which rpm rpm2cpio &>/dev/null; then
    die "Cannot find rpm and rpm2cpio, we cannot proceed."
fi

if ! which ruby &>/dev/null; then
    die "You must have Ruby installed to run this script.  We cannot proceed."
fi

# Source our config file if we have one
[[ -f $HOME/.build-crowbar.conf ]] && \
    . "$HOME/.build-crowbar.conf"

# Look for a local one.
[[ -f build-crowbar.conf ]] && \
    . "build-crowbar.conf"

# Set up our proxies if we were asked to.
if [[ $USE_PROXY = "1" && $PROXY_HOST ]]; then
    proxy_str="http://"
    if [[ $PROXY_PASSWORD && $PROXY_USER ]]; then
        proxy_str+="$PROXY_USER:$PROXY_PASSWORD@"
    elif [[ $PROXY_USER ]]; then
        proxy_str+="$PROXY_USER@"
    fi
    proxy_str+="$PROXY_HOST"
    [[ $PROXY_PORT ]] && proxy_str+=":$PROXY_PORT"
    [[ $no_proxy ]] || no_proxy="localhost,localhost.localdomain,127.0.0.0/8,$PROXY_HOST"
    [[ $http_proxy ]] || http_proxy="$proxy_str/"
    [[ $https_proxy ]] || https_proxy="$http_proxy"
    export no_proxy http_proxy https_proxy
else
    unset no_proxy http_proxy https_proxy
fi

# Always run in verbose mode for now.
VERBOSE=true

# OS to stage Omsahammer on to.  Defaults to CentOS 6.2
[[ $OMSAHAMMER_OS ]] || OMSAHAMMER_OS="centos-6.2"
OS_TO_STAGE="$OMSAHAMMER_OS"
OS_TOKEN="$OS_TO_STAGE"

# Location for caches that should not be erased between runs
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.crowbar-build-cache"

# The directory that we will mount the OS .ISO on .
[[ $IMAGE_DIR ]] || \
    IMAGE_DIR="$CACHE_DIR/$OS_TOKEN/omsahammer-image"

# Location to store .iso images that we use in the build process.
# These are usually OS install DVDs that we will stage Crowbar on to.
[[ $ISO_LIBRARY ]] || ISO_LIBRARY="$CACHE_DIR/iso"

[[ $CHROOT ]] || CHROOT="$CACHE_DIR/$OS_TOKEN/omsahammer-chroot"
sudo rm -rf "$CHROOT"

mkdir -p "$CACHE_DIR" "$IMAGE_DIR" "$CHROOT"

# Location of the Crowbar checkout we are building from.
[[ $CROWBAR_DIR ]] || CROWBAR_DIR="${0%/*}"
[[ $CROWBAR_DIR = /* ]] || CROWBAR_DIR="$currdir/$CROWBAR_DIR"
[[ -f $CROWBAR_DIR/build_crowbar.sh && -d $CROWBAR_DIR/.git ]] || \
    die "$CROWBAR_DIR is not a git checkout of Crowbar!"
export CROWBAR_DIR

# Directory that holds our Omsahammer PXE tree.
[[ $OMSAHAMMER_PXE_DIR ]] || OMSAHAMMER_PXE_DIR="$CACHE_DIR/tftpboot-omsa"

unset CROWBAR_BUILD_PID
# Source our common build functions
. "$CROWBAR_DIR/build_lib.sh" || exit 1
. "$CROWBAR_DIR/test_lib.sh" || exit 1

# Make sure that we actually know how to build the ISO we were asked to
# build.  If we do not, print a helpful error message.
if ! [[ $OS_TO_STAGE && -d $CROWBAR_DIR/$OS_TO_STAGE-extra && \
    -f $CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh ]]; then
    cat <<EOF
You must pass the name of the operating system you want to stage Omsahammer
on to.  Valid choices are:
EOF
cd "$CROWBAR_DIR"
for d in *-extra; do
    [[ -d $d && -f $d/build_lib.sh ]] || continue
    echo "    ${d%-extra}"
done
exit 1
fi

OMSAHAMMER_CHROOT_CACHE="$CACHE_DIR/sledgehammer/$OS_TO_STAGE/chroot_cache"

[[ -f $CROWBAR_DIR/$OS_TO_STAGE-extra/build_sledgehammer_lib.sh ]] || \
    die "Do not know how to build Omsahammer on this OS!"

. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh"

. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_sledgehammer_lib.sh"

debug "Mounting $ISO"
sudo mount -t iso9660 -o loop "$ISO_LIBRARY/$ISO" "$IMAGE_DIR" || \
    die "Could not mount $ISO"

make_chroot
mkdir -p "$OMSAHAMMER_CHROOT_CACHE"
in_chroot mkdir -p /mnt
sudo mount --bind "$ISO_LIBRARY" "$CHROOT/mnt"
sudo mount --bind "$OMSAHAMMER_CHROOT_CACHE" "$CHROOT/$CHROOT_PKGDIR"
setup_sledgehammer_chroot
in_chroot touch /make_omsahammer
in_chroot chmod 777 /make_omsahammer
echo '#!/bin/bash' >>"$CHROOT/make_omsahammer"
if [[ $USE_PROXY = "1" ]]; then
    printf "\nexport no_proxy=%q http_proxy=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/make_omsahammer"
    printf "\nexport NO_PROXY=%q HTTP_PROXY=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/make_omsahammer"
fi
cat >> "$CHROOT/make_omsahammer" <<EOF
set -e
cd /
rm -fr /tftpboot
livecd-iso-to-pxeboot /mnt/OMSA65-CentOS6-x86_64-LiveDVD.iso
/bin/bash
EOF
in_chroot /make_omsahammer
cp -a "$CHROOT/tftpboot" "$CACHE_DIR/tftpboot-omsa"
in_chroot /bin/rm -rf /tftpboot

[[ -f $CACHE_DIR/tftpboot-omsa/initrd0.img ]]
