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

cleanup() {
    # Clean up any stray mounts we may have left behind.
    GREPOPTS=()
    [[ $CACHE_DIR ]] && GREPOPTS=(-e "$CACHE_DIR")
    [[ $IMAGE_DIR && $CACHE_DIR =~ $IMAGE_DIR ]] && GREPOPTS=(-e "$IMAGE_DIR")
    [[ $BUILD_DIR && $CACHE_DIR =~ $BUILD_DIR ]] && GREPOPTS=(-e "$BUILD_DIR")
    if [[ $GREPOPTS ]]; then
	while read dev fs type opts rest; do
	    sudo umount -d -l "$fs"
	done < <(tac /proc/self/mounts |grep "${GREPOPTS[@]}")
    fi
    [[ $webrick_pid && -d /proc/$webrick_pid ]] && kill -9 $webrick_pid
    if [[ $CURRENT_BRANCH ]]; then
	# clean up after outselves from merging branches
	cd "$CROWBAR_DIR"
	git checkout -f "${CURRENT_BRANCH##*/}"
	git branch -D "$THROWAWAY_BRANCH"
	[[ $THROWAWAY_STASH ]] && git stash pop "$THROWAWAY_STASH"
    fi
    for d in "$IMAGE_DIR" "$BUILD_DIR"; do
	[[ -d $d ]] && rm -rf -- "$d"
    done
}

trap cleanup 0 INT QUIT TERM

# Location for caches that should not be erased between runs
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.crowbar-build-cache"

# Location to store .iso images
[[ $ISO_LIBRARY ]] || ISO_LIBRARY="$CACHE_DIR/iso"
[[ $ISO_DEST ]] || ISO_DEST="$PWD"

# Directory that holds our Sledgehammer PXE tree.
[[ $SLEDGEHAMMER_DIR ]] || SLEDGEHAMMER_PXE_DIR="$CACHE_DIR/tftpboot"

# Version for ISO
[[ $VERSION ]] || VERSION="dev"

# Name of the openstack iso we will build
[[ $OPENSTACK_ISO ]] || OPENSTACK_ISO="openstack-${VERSION}.iso"

# Location of the Crowbar checkout we are building from.
[[ $CROWBAR_DIR ]] ||CROWBAR_DIR="${0%/*}"
[[ $SLEDGEHAMMER_DIR ]] || SLEDGEHAMMER_DIR="${CROWBAR_DIR}/../sledgehammer"
[[ $VCS_CLEAN_CMD ]] || VCS_CLEAN_CMD='git clean -f -x -d'

# Arrays holding the additional pkgs, gems, and AMI images we will populate
# Crowbar with.
PKGS=()
GEMS=()
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

shift

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

in_repo() ( cd "$CROWBAR_DIR"; git "$@" )

. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh"

{
    # Make sure only one instance of the ISO build runs at a time.
    # Otherwise you can easily end up with a corrupted image.
    flock 65
    while [[ $1 ]]; do
	case $1 in
	    -m|--merge)
		shift
		while [[ $1 && ! ( $1 = -* ) ]]; do
		    BRANCH_TO_MERGE=$(in_repo check-ref-format --branch "$1") || die "$1 is not a git branch!"
		    shift
		    if [[ ! $CURRENT_BRANCH ]]; then
			CURRENT_BRANCH=$(in_repo symbolic-ref HEAD) || die "Not on a branch we can merge with!"
			THROWAWAY_BRANCH="build-throwaway-$$-$RANDOM"
			REPO_PWD="$PWD"
			if [[ ! $(in_repo status) =~ working\ directory\ clean ]]; then
			    THROWAWAY_STASH=$(in_repo stash create)
			fi
			in_repo checkout -b "$THROWAWAY_BRANCH"
			if [[ $THROWAWAY_STASH ]]; then
			    in_repo stash apply "$THROWAWAY_STASH"
			    in_repo commit -a -m "Applying $THROWAWAY_STASH to $THROWAWAY_BRANCH"
			fi
		    fi
		    in_repo merge "$BRANCH_TO_MERGE" || die "Merge of $BRANCH_TO_MERGE failed, fix things up and continue"
		done
		;;
	    update-cache) shift; need_update=true;;
	    *) 	die "Unknown command line parameter $1";;
	esac
    done
		    

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
    [[ $CHROOT ]] || CHROOT="$CACHE_DIR/$OS_TOKEN/chroot"
    [[ $BUILD_DIR ]] || \
	BUILD_DIR="$(mktemp -d "$CACHE_DIR/$OS_TOKEN/build-XXXXX")"
    [[ $IMAGE_DIR ]] || \
	IMAGE_DIR="$CACHE_DIR/$OS_TOKEN/image-${BUILD_DIR##*-}"

    # Directories where we cache our pkgs, gems, and ami files
    [[ $PKG_CACHE ]] || PKG_CACHE="$CACHE_DIR/$OS_TOKEN/pkgs"
    [[ $GEM_CACHE ]] || GEM_CACHE="$CACHE_DIR/gems"
    [[ $AMI_CACHE ]] || AMI_CACHE="$CACHE_DIR/amis"

    # Directory where we will look for our package lists
    [[ $PACKAGE_LISTS ]] || PACKAGE_LISTS="$BUILD_DIR/extra/packages"

    # Tree-ish to check out in the build-cache"
    [[ $CACHE_REVISION ]] || CACHE_REVISION="master"

    # Proxy Variables
    [[ $USE_PROXY ]] || USE_PROXY=0
    [[ $PROXY_HOST ]] || PROXY_HOST=""
    [[ $PROXY_PORT ]] || PROXY_PORT=""
    [[ $PROXY_USER ]] || PROXY_USER=""
    [[ $PROXY_PASSWORD ]] || PROXY_PASSWORD=""
    [[ $WEBRICK_IP ]] || WEBRICK_IP="127.0.0.1"
    [[ $WEBRICK_BIND ]] || WEBRICK_BIND="127.0.0.1"

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
    clean_dirs "$IMAGE_DIR" "$BUILD_DIR"

    (cd "$CROWBAR_DIR"; $VCS_CLEAN_CMD)

    # Mount our ISO for the build process.
    debug "Mounting $ISO"
    sudo mount -t iso9660 -o loop "$ISO_LIBRARY/$ISO" "$IMAGE_DIR" || \
	die "Could not mount $ISO"

    # Make additional directories we will need.
    for d in discovery extra ami updates ; do
	mkdir -p "$BUILD_DIR/$d"
    done

    # Copy over the Crowbar bits and their prerequisites
    debug "Staging extra Crowbar bits"
    cp -r "$CROWBAR_DIR/$OS_TOKEN-extra"/* "$BUILD_DIR/extra"
    cp -r "$CROWBAR_DIR/change-image"/* "$BUILD_DIR"

    # Make sure we have the right branch of the cache checked out
    (cd "$CACHE_DIR"; git checkout -f "$CACHE_REVISION")

    # If we were asked to update our cache, do it.
    maybe_update_cache "$@"
    
    # Copy our extra pkgs, gems, and amis over
    debug "Copying pkgs, gems, and amis"
    copy_pkgs "$IMAGE_DIR" "$PKG_CACHE" "$BUILD_DIR/extra/pkgs"
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
    # Find files and directories that mkisofs will complain about.
    # Do just top-level overlapping directories for now.
    for d in $(cat <(cd "$BUILD_DIR"; find -maxdepth 1 -type d ) \
	           <(cd "$IMAGE_DIR"; find -maxdepth 1 -type d) | \
	           sort |uniq -d); do
	[[ $d = . ]] && continue
	d=${d#./}
	# Copy contents of the found directories into $BUILD_DIR, taking care
	# to not clobber existing files.
	mkdir -p "$BUILD_DIR/$d"
	# We could also use cp -n, but rhel5 and centos5 do not understand it.
	rsync -a --ignore-existing "$IMAGE_DIR/$d/." "$BUILD_DIR/$d/."
	chmod -R u+wr "$BUILD_DIR/$d"
	# Bind mount an empty directory on the $IMAGE_DIR instance.
	sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/$d"
    done
    (   cd "$BUILD_DIR"
	rm -f isolinux/boot.cat
	find -name '.svn' -type d -exec rm -rf '{}' ';' 2>/dev/null >/dev/null
	mkdir -p $ISO_DEST
	mkisofs -r -V "Crowbar $VERSION DVD" -cache-inodes -J -l -quiet \
	    -b isolinux/isolinux.bin -c isolinux/boot.cat \
	    -no-emul-boot --boot-load-size 4 -boot-info-table \
	    -o "$ISO_DEST/$OPENSTACK_ISO" "$IMAGE_DIR" "$BUILD_DIR" ) || \
	    die "There was a problem building our ISO."
 
    echo "$(date '+%F %T %z'): Finshed. Image at $ISO_DEST/$OPENSTACK_ISO"
} 65> /tmp/.build_crowbar.lock
