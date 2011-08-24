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
# Author: VictorLowther
#

# This script expects to be able to run certian commands as root.
# Either run it as a user who can sudo to root, or give the user
# you are running it as the following sudo rights:
# crowbar-tester ALL = NOPASSWD: /bin/mount, /bin/umount, /usr/sbin/debootstrap, /bin/cp, /usr/sbin/chroot

# When running this script for the first time, it will automatically create a
# cache directory and try to populate it with all the build dependencies.
# After that, if you need to pull in new dependencies, you will need to
# call the script with the update-cache parameter.

[[ $DEBUG ]] && {
    set -x
    export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
}

export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin"

# Location for caches that should not be erased between runs
CACHE_DIR="$HOME/.crowbar-build-cache"

# Location to store .iso images
ISO_LIBRARY="$CACHE_DIR/iso"
ISO_DEST="$PWD"

# Location that holds temporary mount images of our.isos
# if we need to extract them onto our target image.
IMAGE_DIR="$CACHE_DIR/image"

# Location we will stage the new openstack iso at.
BUILD_DIR="$CACHE_DIR/build"

# Directory that holds our Sledgehammer PXE tree.
SLEDGEHAMMER_PXE_DIR="$CACHE_DIR/tftpboot"

# Version for ISO
VERSION="dev"

# Name of the openstack iso we will build
OPENSTACK_ISO="openstack-${VERSION}.iso"

# Location of the Crowbar checkout we are building from.
CROWBAR_DIR="${0%/*}"
SLEDGEHAMMER_DIR="${CROWBAR_DIR}/../sledgehammer"
VCS_CLEAN_CMD='git clean -f -x -d'

# Arrays holding the additional pkgs, gems, and AMI images we will populate
# Crowbar with.
PKGS=("apt-utils",
"wget",
"curl",
"libcurl3",
"bison",
"build-essential",
"zlib1g-dev",
"libssl-dev",
"libreadline5-dev",
"libxml2",
"libxml2-dev",
"libxslt1.1",
"libxslt1-dev",
"git-core",
"sqlite3",
"libsqlite3-ruby",
"libsqlite3-dev",
"unzip",
"zip",
"ruby-dev",
"libmysql-ruby",
"libmysqlclient-dev",
"libcurl4-openssl-dev",
"libpq-dev",
"python-software-properties",
"mysql-client",
"mysql-server",
"build-essential",
"libssl-dev",
"zlib1g-dev",
"libreadline5-dev",
"libxml2-dev",
"libpq-dev",
"postgresql",
"erlang-nox",
"libreadline5-dev")
GEMS=("vmc", "nats", "bundler", "rake")
AMIS=("http://uec-images.ubuntu.com/releases/11.04/release/ubuntu-11.04-server-uec-amd64.tar.gz")


die() { echo "$(date '+%F %T %z'): $*" >&2; exit 1; }
debug() { echo "$(date '+%F %T %z'): $*" >&2; }
clean_dirs() {
    local d=''
    for d in "$@"; do
	(   mkdir -p "$d"
	    cd "$d"
	    chmod -R u+w .
	    rm -rf * )
    done
}

OS_TO_STAGE="${1-ubuntu-10.10}"

if ! [[ $OS_TO_STAGE && -d $CROWBAR_DIR/$OS_TO_STAGE-extra && \
    -f $CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh ]]; then
    cat <<EOF
You must pass the name of the operating system you want to stage Crowbar
on to.  Valid choices are:
EOF
    cd "$CROWBAR_DIR"
    for d in *-extra; do
	[[ -d $d && -f $d/build_lib.sh ]] || continue
	echo "    ${d%-extra}"
    done
    exit 1
fi

. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh"

{
    # Make sure only one instance of the ISO build runs at a time.
    # Otherwise you can easily end up with a corrupted image.
    flock 65

    # Source our config file if we have one
    [[ -f $HOME/.build-crowbar.conf ]] && \
	. "$HOME/.build-crowbar.conf"

    # Look for a local one.
    [[ -f build-crowbar.conf ]] && \
	. "build-crowbar.conf"

    # Finalize where we expect to find our caches and out chroot.
    # If they were set in one of the conf files, don't touch them.

    # The directory we perform a minimal install into if we need
    # to refresh our gem or pkg caches
    [[ $CHROOT ]] || CHROOT="$CACHE_DIR/$OS_TOKEN.chroot"

    # Directories where we cache our pkgs, gems, and ami files
    [[ $PKG_CACHE ]] || PKG_CACHE="$CACHE_DIR/$OS_TOKEN/pkgs"
    [[ $GEM_CACHE ]] || GEM_CACHE="$CACHE_DIR/gems"
    [[ $AMI_CACHE ]] || AMI_CACHE="$CACHE_DIR/amis"

    # directory we will mount the .iso on to extract packages.
    [[ $IMG_MNTPT ]] || IMG_MNTPT="$IMAGE_DIR/${ISO%.iso}"

    # Make any directories we don't already have
    for d in "$PKG_CACHE" "$GEM_CACHE" "$ISO_LIBRARY" "$ISO_DEST" \
	"$IMAGE_DIR" "$BUILD_DIR" "$AMI_CACHE" \
	"$SLEDGEHAMMER_PXE_DIR" "$CHROOT"; do
	mkdir -p "$d"
    done

    # Make sure Sledgehammer has already been built and pre-staged.
    if ! [[ -f $SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz || \
	-f $SLEDGEHAMMER_PXE_DIR/initrd0.img ]]; then
	echo "Slegehammer TFTP image missing!"
	echo "Please build Sledgehammer from $SLEDGEHAMMER_DIR before building Crowbar."
	exit 1
    fi  
  
    # make sure we have the AMIs we want
    for ami in "${AMIS[@]}"; do
	[[ -f $AMI_CACHE/${ami##*/} ]] && continue
	echo "$(date '+%F %T %z'): Downloading and caching $ami"
	curl -o "$AMI_CACHE/${ami##*/}" "$ami" || \
	    die "Could not download $ami"
    done 

    # Fetch the OS ISO if we need to.
    [[ -f $ISO_LIBRARY/$ISO ]] || fetch_os_iso

    # Start with a clean slate.
    clean_dirs "$IMG_MNTPT" "$BUILD_DIR"

    (cd "$CROWBAR_DIR"; $VCS_CLEAN_CMD)
    # Copy everything off the ISO to our build directory
    debug "Copying off $ISO"
    sudo mount -t iso9660 -o loop "$ISO_LIBRARY/$ISO" "$IMG_MNTPT" || \
	die "Could not mount $ISO"
    cp -rT "$IMG_MNTPT" "$BUILD_DIR"
    sudo umount -d "$IMG_MNTPT"

    # Make everything writable again.
    chmod -R u+w "$BUILD_DIR"

    # Make additional directories we will need.
    for d in discovery extra ami updates ; do
	mkdir -p "$BUILD_DIR/$d"
    done

    # Copy over the Crowbar bits and their prerequisites
    debug "Staging extra Crowbar bits"
    cp -r "$CROWBAR_DIR/$OS_TOKEN-extra"/* "$BUILD_DIR/extra"
    cp -r "$CROWBAR_DIR/change-image"/* "$BUILD_DIR"

    # If we were asked to update our cache, do it.
    maybe_update_cache "$@"

    # Copy our extra pkgs, gems, and amis over
    debug "Copying pkgs, gems, and amis"
    copy_pkgs "$BUILD_DIR/pool" "$PKG_CACHE" "$BUILD_DIR/extra/pkgs"
    cp -r "$GEM_CACHE" "$BUILD_DIR/extra"
    cp -r "$AMI_CACHE/." "$BUILD_DIR/ami/."
    
    reindex_packages
    # Store off the version
    echo "$VERSION" >> "$BUILD_DIR/dell/Version"
   
    final_build_fixups
 
    # Copy over the Sledgehammer bits
    debug "Copying over Sledgehammer bits"
    for d in "$CROWBAR_DIR/"updates*; do
	[[ -d $d ]] || continue
	cp -r "$d"/* "$BUILD_DIR/updates"
    done
    # If we need to copy over a new Sledgehammer image, do so.
    if [[ $SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz -nt \
	$SLEDGEHAMMER_PXE_DIR/initrd0.img ]]; then
	(   cd $SLEDGEHAMMER_PXE_DIR
	    debug "Extracting new Sledgehammer TFTP boot image"
	    rm -rf .
	    cd ..
	    tar xzf "$SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz"
	    rm -f "$SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz"
	)
    fi
    cp -a "$SLEDGEHAMMER_PXE_DIR"/* "$BUILD_DIR/discovery"

    # Make our image
    debug "Creating new ISO"
    (   cd "$BUILD_DIR"
	find -name '.svn' -type d -exec rm -rf '{}' ';' 2>/dev/null >/dev/null
	mkdir -p $ISO_DEST
	mkisofs -r -V "Crowbar $VERSION DVD" -cache-inodes -J -l -quiet \
	    -b isolinux/isolinux.bin -c isolinux/boot.cat \
	    -no-emul-boot --boot-load-size 4 -boot-info-table \
	    -o "$ISO_DEST/$OPENSTACK_ISO" "$BUILD_DIR" ) || \
	    die "There was a problem building our ISO."
    echo "$(date '+%F %T %z'): Finshed. Image at $ISO_DEST/$OPENSTACK_ISO"
} 65> /tmp/.build_crowbar.lock
