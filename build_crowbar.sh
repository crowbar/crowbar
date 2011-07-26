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
    export PS4='(${nodename:-none})${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
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

# Version of Ubuntu we are building openstack on to.
UBUNTU_VERSION=10.10
UBUNTU_CODENAME=maverick
PPAS=("openstack-release/2011.2")

# Server to download the mirror from if we need to.
UBUNTU_ISO_MIRROR="http://mirror.anl.gov/pub"

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

# Arrays holding the additional debs, gems, and AMI images we will populate
# Crowbar with.
DEBS=()
GEMS=()
AMIS=("http://uec-images.ubuntu.com/releases/11.04/release/ubuntu-11.04-server-uec-amd64.tar.gz")


die() { shift; echo "$(date '+%F %T %z'): $*" >&2; exit 1; }
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

which debootstrap &>/dev/null || die "debootstrap must be installed! Exiting."
which dpkg-scanpackages &>/dev/null || die "build-essential must be installed! Exiting."


update_caches() {
    # Hold a list of directories we will need to umount
    TO_UMOUNT=()
    
    # Make sure our cache directories exist.
    mkdir -p "$DEB_CACHE" 
    mkdir -p "$GEM_CACHE"

    # A little helper function for doing bind mounts.
    bind_mount() {
	TO_UMOUNT=("${TO_UMOUNT[@]}" "$2")
	[[ -d $2 ]] || mkdir -p "$2"
	grep -q "$2" /proc/self/mounts || sudo mount --bind "$1" "$2"
    }
    
    # A little helper for running commands in the chroot.
    in_chroot() { sudo -H chroot "$UBUNTU_CHROOT" "$@"; }

    # second, debootstrap a minimal install of our target version of
    # Ubuntu to ensure that we don't interfere with the host's package cache.
    debug "Making package-fetching chroot"
    mkdir -p "$UBUNTU_CHROOT"
    sudo mount -t tmpfs -o size=1G none "$UBUNTU_CHROOT" 
    sudo debootstrap "$UBUNTU_CODENAME" "$UBUNTU_CHROOT" \
	"file://$BUILD_DIR" || \
	die 1 "Could not bootstrap our scratch target!"
    # mount some important directories for the chroot
    for d in proc sys dev dev/pts; do
	bind_mount "/$d" "$UBUNTU_CHROOT/$d"
    done
    # make sure the chroot can resolve hostnames
    sudo cp /etc/resolv.conf "$UBUNTU_CHROOT/etc/resolv.conf"

    # Make sure we are using a correctly prepopulated sources.list.
    sudo cp "$BUILD_DIR/extra/sources.list" \
	"$UBUNTU_CHROOT/etc/apt/sources.list"
    
    # if we have deb caches, copy them back in to save time on the downloads.
    sudo cp -a "$DEB_CACHE/." "$UBUNTU_CHROOT/var/cache/apt/archives/."
    
    debug "Fetching needed packages"
    # update, add infrastructure for adding PPAs, 
    # add additional PPAs, and update again.
    in_chroot /usr/bin/apt-get -y --force-yes --allow-unauthenticated update
    in_chroot /usr/bin/apt-get -y --force-yes --allow-unauthenticated install \
	python-software-properties
    for ppa in "${PPAS[@]}"; do
	in_chroot apt-add-repository "ppa:$ppa"
    done
    # Get the key for the Opscode repo we are grabbing Chef bits from.
    wget -qO - http://apt.opscode.com/packages@opscode.com.gpg.key | \
	in_chroot /usr/bin/apt-key add -
    in_chroot /usr/bin/apt-get -y --force-yes --allow-unauthenticated update

    # Download all the packages apt thinks we will need.
    in_chroot /usr/bin/apt-get -y --force-yes \
	--allow-unauthenticated --download-only install "${DEBS[@]}"
    # actually install ruby1.8-dev and gem and their deps.
    in_chroot /usr/bin/apt-get -y --force-yes \
	--allow-unauthenticated install ruby1.8-dev rubygems1.8 build-essential
    # install the gems we will need and all their dependencies
    # We will get some build failures, but at this point we don't care because
    # we are just caching the gems for the real install.
    debug "Fetching Gems"
    echo "There may be build failures here, we can safely ignore them."
    gem_re='([^0-9].*)-([0-9].*)'
    for gem in "${GEMS[@]}"; do
	if [[ $gem =~ $gem_re ]]; then
	    echo "${BASH_REMATCH[*]}"
	    gemname="${BASH_REMATCH[1]}"
	    gemver="${BASH_REMATCH[2]}"
	else
	    gemname="$gem"
	    gemver=''
	fi
	gemopts=(install --no-ri --no-rdoc)
	[[ $gemver ]] && gemopts+=(--version "= ${gemver}")
	in_chroot /usr/bin/gem "${gemopts[@]}" "$gemname"
    done
    debug "Saving downloaded packages"
    # Save our updated gems and debs in the cache for later.
    cp -a "$UBUNTU_CHROOT/var/cache/apt/archives/." "$DEB_CACHE/." 
    cp -a "$UBUNTU_CHROOT/var/lib/gems/1.8/cache/." "$GEM_CACHE/."
    sync

    debug "Cleaning up mounts"
    # umount all the stuff we have mounted for the chroot.
    while grep -q "$UBUNTU_CHROOT" /proc/self/mounts; do 
	for m in "${TO_UMOUNT[@]}"; do sudo umount "$m"; sleep 1; done
	sudo umount "$UBUNTU_CHROOT"
    done
}

copy_debs() {
    # $1 = pool directory to build initial list of excludes against
    # $2 = directory to copy from
    # $3 = directory to copy to.
    
    # First, a couple of hashes to hold deb => revision values
    declare -A deb_pool
    declare -A dest_pool
    
    local debname=''
    local debarr=()
    local debs_to_copy=()
    
    
    # Scan through our pool to find debs we can easily omit.
    while read debname; do
	[[ -f $debname  && $debname = *.deb ]] || continue
	debname="${debname##*/}"
	debarr=(${debname//_/ }) # split into (name version arch)
	deb_pool["${debarr[0]}"]="${debarr[1]}"
    done < <(find "$1" -name '*.deb')
    
    (   
	cd "$2"
	for deb in *; do
	    [[ -f $deb && $deb = *.deb ]] || continue
	    debname="${deb##*/}" # don't care about the source path
	    debname="${debname%_*.deb}" # don't care about the arch
	    debver="${debname#*_}"
	    debname="${debname%_*}"
	    # First, have we already copied another version of this 
	    # deb?  If so, decide whether to copy it or not.
	    if [[ ${dest_pool["$debname"]} ]]; then
		# We have seen it.  If the version we already copied 
		# is older than this one, queue this for copying instead.
		# This relies on * expansion returning names in order.
		if [[ ${dest_pool["$debname"]} < $debver ]]; then
		    debug "Omitting ${debname}_${dest_pool[$debname]} in favor of ${debname}_${debver}"
		    debs_to_copy[$((${#debs_to_copy[@]} - 1))]="$deb"
		    dest_pool["$debname"]="$debver"
		fi
	    # Second check and see if it is already inthe install pool.
	    # If it is and it is the same or lesser version, don't copy it
	    elif [[ ${deb_pool["$debname"]} ]]; then
		if [[ ${deb_pool["$debname"]} < $debver ]]; then
		    debs_to_copy+=("$deb")
		else
		    debug "${debname}_${debver} in CD pool, omitting"
		fi
	    else
		# It is not already in deb_pool or dest_pool, copy it.
		debs_to_copy+=("$deb")
		debug "Will copy ${debname}_${debver}"
	    fi
	done
	# Now, we have a list of debs to copy, so do it. 
	mkdir -p "$3"
	cp -t "$3" "${debs_to_copy[@]}" 
    )
}

maybe_update_cache() {
    local pkgfile deb gem pkg_type rest need_update _pwd 
    debug "Processing package lists"
    # Zero out our sources.list
    > "$BUILD_DIR/extra/sources.list"
    # Download and stash any extra files we may need
    # First, build our list of repos, ppas, debs, and gems
    for pkgfile in "$BUILD_DIR/extra/packages/"*.list; do
	[[ -f $pkgfile ]] || continue
	while read pkg_type rest; do
	    case $pkg_type in
		repository) 
		    echo "${rest%%#*}" >> "$BUILD_DIR/extra/sources.list";;
		ppas) PPAS+=(${rest%%#*});;
		debs) DEBS+=(${rest%%#*});;
		gems) GEMS+=(${rest%%#*});;
	    esac
	done <"$pkgfile"
    done
    
    _pwd=$PWD
    cd "$DEB_CACHE"
    # second, verify that the debs we need are in the cache.
    for deb in "${DEBS[@]}"; do
	[[ $(echo "$deb"*.deb) != "$deb*.deb" ]] || {
	    need_update=true
	    break
	}
    done
    
    cd "$GEM_CACHE"
    # third, verify that the gems we need are in the cache
    for gem in "${GEMS[@]}"; do
	[[ $(echo "$gem"*.gem) != "$gem*.gem" ]] || {
	    need_update=true
	    break
	}
    done
    cd "$_pwd"

    if [[ $need_update = true || \
	( ! -d $DEB_CACHE ) || $* =~ update-cache ]]; then
	update_caches
    else
	return 0
    fi
}

for cmd in sudo chroot debootstrap mkisofs; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done

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

    # The directory we perform a minimal install of Ubuntu into if we need
    # to refresh our gem or deb caches
    [[ $UBUNTU_CHROOT ]] || UBUNTU_CHROOT="$CACHE_DIR/$UBUNTU_CODENAME.chroot"

    # Directories where we cache our debs, gems, and ami files
    [[ $DEB_CACHE ]] || DEB_CACHE="$CACHE_DIR/$UBUNTU_CODENAME/debs"
    [[ $GEM_CACHE ]] || GEM_CACHE="$CACHE_DIR/gems"
    [[ $AMI_CACHE ]] || AMI_CACHE="$CACHE_DIR/amis"

    # The name of the Ubuntu iso we are using as a base.
    [[ $UBUNTU_ISO ]] || UBUNTU_ISO="ubuntu-$UBUNTU_VERSION-server-amd64.iso"

    # directory we will mount the Ubuntu .iso on to extract packages.
    [[ $UBUNTU_DIR ]] || UBUNTU_DIR="$IMAGE_DIR/${UBUNTU_ISO%.iso}"

    
    # Make any directories we don't already have
    for d in "$ISO_LIBRARY" "$ISO_DEST" "$IMAGE_DIR" "$BUILD_DIR" "$AMI_CACHE" \
	"$SLEDGEHAMMER_PXE_DIR" "$UBUNTU_CHROOT"; do
	mkdir -p "$d"
    done

    # Make sure Sledgehammer has already been built and pre-staged.
    if ! [[ -f $SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz || \
	-f $SLEDGEHAMMER_PXE_DIR/initrd0.img ]]; then
	echo "Slegehammer TFTP image missing!"
	echo "Please build Sledgehammer from $SLEDGEHAMMER_DIR before buildin gCrowbar."
	exit 1
    fi  
  
    # make sure we have the AMIs we want
    for ami in "${AMIS[@]}"; do
	[[ -f $AMI_CACHE/${ami##*/} ]] && continue
	echo "$(date '+%F %T %z'): Downloading and caching $ami"
	curl -o "$AMI_CACHE/${ami##*/}" "$ami" || \
	    die 1 "Could not download $ami"
    done 

    # Try and download our ISO if we don't already have it
    [[ -f $ISO_LIBRARY/$UBUNTU_ISO ]] || {
	echo "$(date '+%F %T %z'): Downloading and caching $UBUNTU_ISO"
	curl -o "$ISO_LIBRARY/$UBUNTU_ISO" \
	"$UBUNTU_ISO_MIRROR/ubuntu-iso/CDs/$UBUNTU_VERSION/$UBUNTU_ISO" || \
	die 1 "Missing our Ubuntu source image"
    }

    # Start with a clean slate.
    clean_dirs "$UBUNTU_DIR" "$BUILD_DIR"

    (cd "$CROWBAR_DIR"; $VCS_CLEAN_CMD)
    # Copy everything off the Ubuntu ISO to our build directory
    debug "Copying Ubuntu off $UBUNTU_ISO"
    sudo mount -t iso9660 -o loop "$ISO_LIBRARY/$UBUNTU_ISO" "$UBUNTU_DIR" || \
	die "Could not mount $UBUNTU_ISO"
    cp -rT "$UBUNTU_DIR" "$BUILD_DIR"
    sudo umount -d "$UBUNTU_DIR"

    # Make everything writable again.
    chmod -R u+w "$BUILD_DIR"

    # Make additional directories we will need.
    for d in discovery extra ami updates ; do
	mkdir -p "$BUILD_DIR/$d"
    done

    # Copy over the Crowbar bits and their prerequisites
    debug "Staging extra Crowbar bits"
    cp -r "$CROWBAR_DIR/ubuntu-$UBUNTU_VERSION-extra"/* "$BUILD_DIR/extra"
    cp -r "$CROWBAR_DIR/change-image"/* "$BUILD_DIR"

    # If we were asked to update our cache, do it.
    maybe_update_cache "$@"

    # Copy our extra debs, gems, and amis over
    debug "Copying debs, gems, and amis"
    copy_debs "$BUILD_DIR/pool" "$DEB_CACHE" "$BUILD_DIR/extra/debs"
    cp -r "$GEM_CACHE" "$BUILD_DIR/extra"
    cp -r "$AMI_CACHE/." "$BUILD_DIR/ami/."
    
    # Make our new packages repository.
    (   cd "$BUILD_DIR/extra" 
	debug "Recreating Packages.gz"
	dpkg-scanpackages debs /dev/null 2>/dev/null |gzip -9 >Packages.gz)
    # Store off the version
    echo "$VERSION" >> "$BUILD_DIR/dell/Version"
   
    # Fix up the initrd
    (   cd "$CROWBAR_DIR/initrd"
	debug "Fixing up initrd"
	[[ -d scratch ]] && rm -rf scratch
	mkdir scratch
        # Grab _all_ the nic drivers. We probably don't need them,
        # but a little paranoia never hurt anyone.
	(   cd scratch;
	    debug "Adding all nic drivers"
	    for udeb in "$BUILD_DIR/pool/main/l/linux/"nic-*-generic-*.udeb; do
		ar x "$udeb"
		tar xzf data.tar.gz
		rm -rf debian-binary *.tar.gz
	    done 
            # Make sure installing off a USB connected DVD will work
	    debug "Adding USB connected DVD support"
	    mkdir -p var/lib/dpkg/info
	    cp ../cdrom-detect.postinst var/lib/dpkg/info
	    # Append our new gzipped CPIO archive onto the old one.
	    find . |cpio --create --format=newc --owner root:root 2>/dev/null | \
	    gzip -9 >> "$BUILD_DIR/install/initrd.gz" )
	rm -rf scratch )
     
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
	    die 1 "There was a problem building our ISO."
    echo "$(date '+%F %T %z'): Finshed. Image at $ISO_DEST/$OPENSTACK_ISO"
} 65> /tmp/.build_crowbar.lock
