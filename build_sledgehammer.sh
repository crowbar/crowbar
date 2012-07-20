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
fi

# Always run in verbose mode for now.
VERBOSE=true

# OS to stage Sledgehammer on to.  Defaults to CentOS 6.2
[[ $SLEDGEHAMMER_OS ]] || SLEDGEHAMMER_OS="centos-6.2"
OS_TO_STAGE="$SLEDGEHAMMER_OS"
OS_TOKEN="$OS_TO_STAGE"

# Location for caches that should not be erased between runs
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.crowbar-build-cache"

# The directory that we will mount the OS .ISO on .
[[ $IMAGE_DIR ]] || \
    IMAGE_DIR="$CACHE_DIR/$OS_TOKEN/sledgehammer-image"

# Location to store .iso images that we use in the build process.
# These are usually OS install DVDs that we will stage Crowbar on to.
[[ $ISO_LIBRARY ]] || ISO_LIBRARY="$CACHE_DIR/iso"

[[ $CHROOT ]] || CHROOT="$CACHE_DIR/$OS_TOKEN/sledgehammer-chroot"
sudo rm -rf "$CHROOT"

mkdir -p "$CACHE_DIR" "$IMAGE_DIR" "$CHROOT"

# Location of the Crowbar checkout we are building from.
[[ $CROWBAR_DIR ]] || CROWBAR_DIR="${0%/*}"
[[ $CROWBAR_DIR = /* ]] || CROWBAR_DIR="$currdir/$CROWBAR_DIR"
[[ -f $CROWBAR_DIR/build_crowbar.sh && -d $CROWBAR_DIR/.git ]] || \
    die "$CROWBAR_DIR is not a git checkout of Crowbar!"
export CROWBAR_DIR

# Directory that holds our Sledgehammer PXE tree.
[[ $SLEDGEHAMMER_PXE_DIR ]] || SLEDGEHAMMER_PXE_DIR="$CACHE_DIR/tftpboot"

unset CROWBAR_BUILD_PID
# Source our common build functions
. "$CROWBAR_DIR/build_lib.sh" || exit 1
. "$CROWBAR_DIR/test_lib.sh" || exit 1

# Make sure that we actually know how to build the ISO we were asked to
# build.  If we do not, print a helpful error message.
if ! [[ $OS_TO_STAGE && -d $CROWBAR_DIR/$OS_TO_STAGE-extra && \
    -f $CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh ]]; then
    cat <<EOF
You must pass the name of the operating system you want to stage Sledgehammer
on to.  Valid choices are:
EOF
cd "$CROWBAR_DIR"
for d in *-extra; do
    [[ -d $d && -f $d/build_lib.sh ]] || continue
    echo "    ${d%-extra}"
done
exit 1
fi

SLEDGEHAMMER_CHROOT_CACHE="$CACHE_DIR/sledgehammer/$OS_TO_STAGE/chroot_cache"
SLEDGEHAMMER_LIVECD_CACHE="$CACHE_DIR/sledgehammer/$OS_TO_STAGE/livecd_cache"

[[ -f $CROWBAR_DIR/$OS_TO_STAGE-extra/build_sledgehammer_lib.sh && \
    -f $CROWBAR_DIR/$OS_TO_STAGE-extra/sledgehammer.ks ]] || \
    die "Do not know how to build Sledgehammer on this OS!"

. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh"
. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_sledgehammer_lib.sh"

setup_sledgehammer_chroot
sudo cp "$CROWBAR_DIR/$OS_TO_STAGE-extra/sledgehammer.ks" "$CHROOT/mnt"
sudo cp "$CROWBAR_DIR/sledgehammer-common/"* "$CHROOT/mnt"
mkdir -p "$SLEDGEHAMMER_CHROOT_CACHE"
mkdir -p "$SLEDGEHAMMER_LIVECD_CACHE"
in_chroot mkdir -p /mnt/cache
sudo mount --bind "$SLEDGEHAMMER_CHROOT_CACHE" "$CHROOT/$CHROOT_PKGDIR"
sudo mount --bind "$SLEDGEHAMMER_LIVECD_CACHE" "$CHROOT/mnt/cache"
in_chroot touch /mnt/make_sledgehammer
in_chroot chmod 777 /mnt/make_sledgehammer
echo '#!/bin/bash' >>"$CHROOT/mnt/make_sledgehammer"
if [[ $USE_PROXY = "1" ]]; then
    printf "\nexport no_proxy=%q http_proxy=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/mnt/make_sledgehammer"
    printf "\nexport NO_PROXY=%q HTTP_PROXY=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/mnt/make_sledgehammer"
fi
cat >> "$CHROOT/mnt/make_sledgehammer" <<EOF
set -e
cd /mnt
livecd-creator --config=sledgehammer.ks --cache=./cache -f sledgehammer
rm -fr /mnt/tftpboot
livecd-iso-to-pxeboot sledgehammer.iso
/bin/rm /mnt/sledgehammer.iso
EOF
in_chroot ln -s /proc/self/mounts /etc/mtab
in_chroot /mnt/make_sledgehammer
cp -a "$CHROOT/mnt/tftpboot" "$CACHE_DIR/"
in_chroot /bin/rm -rf /mnt/tftpboot

[[ -f $CACHE_DIR/tftpboot/initrd0.img ]]
