#!/bin/bash
# This file is sourced by build_crowbar.sh when you want to build Crowbar 
# using Ubuntu 10.10 as the base OS.  It includes all Ubuntu 10.10 specific
# build routines.

# OS information for the OS we are building openstack on to.
OS=ubuntu
OS_VERSION=10.10
OS_TOKEN="$OS-$OS_VERSION"
OS_CODENAME=maverick

# Server to download the mirror from if we need to.
ISO_MIRROR="http://mirror.anl.gov/pub"

# HTTP/HTTPS proxy  details (optional)
# i.e. http://proxyhost:port/ ...  https://username:password@proxyhost:port/
HTTP_PROXY_ADDR=""

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="ubuntu-$OS_VERSION-server-amd64.iso"

fetch_os_iso() {
    # Try and download our ISO if we don't already have it
    echo "$(date '+%F %T %z'): Downloading and caching $ISO"
    curl -o "$ISO_LIBRARY/$ISO" \
	"$ISO_MIRROR/ubuntu-iso/CDs/$OS_VERSION/$ISO" || \
	die 1 "Missing our source image"
}

update_caches() {
    # Hold a list of directories we will need to umount
    TO_UMOUNT=()
    
    # A little helper function for doing bind mounts.
    bind_mount() {
	TO_UMOUNT=("${TO_UMOUNT[@]}" "$2")
	[[ -d $2 ]] || mkdir -p "$2"
	grep -q "$2" /proc/self/mounts || sudo mount --bind "$1" "$2"
    }
    
    # A little helper for running commands in the chroot.
    in_chroot() { sudo -H chroot "$CHROOT" "$@"; }

    # second, debootstrap a minimal install of our target version of
    # Ubuntu to ensure that we don't interfere with the host's package cache.
    debug "Making package-fetching chroot"
    mkdir -p "$CHROOT"
    sudo mount -t tmpfs -o size=1G none "$CHROOT" 
    sudo debootstrap "$OS_CODENAME" "$CHROOT" \
	"file://$BUILD_DIR" || \
	die 1 "Could not bootstrap our scratch target!"
    # mount some important directories for the chroot
    for d in proc sys dev dev/pts; do
	bind_mount "/$d" "$CHROOT/$d"
    done

    # set HTTP/HTTPS proxies
    if [ "" != "$HTTP_PROXY_ADDR" ]
    then
      sudo echo "export http_proxy=$HTTP_PROXY_ADDR" >> "$CHROOT/root/.bashrc"
      sudo echo "export https_proxy=$HTTP_PROXY_ADDR" >> "$CHROOT/root/.bashrc"

      sudo mkdir -p "$CHROOT/etc/apt/apt.conf.d/"
      sudo echo "Acquire::http::proxy \"$HTTP_PROXY_ADDR\";" >> "$CHROOT/etc/apt/apt.conf.d/proxy"
      sudo echo "Acquire::https::proxy \"$HTTP_PROXY_ADDR\";" >> "$CHROOT/etc/apt/apt.conf.d/proxy"
    fi

    # make sure the chroot can resolve hostnames
    sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"

    # Make sure we are using a correctly prepopulated sources.list.
    sudo cp "$BUILD_DIR/extra/sources.list" \
	"$CHROOT/etc/apt/sources.list"
    
    # if we have deb caches, copy them back in to save time on the downloads.
    sudo cp -a "$PKG_CACHE/." "$CHROOT/var/cache/apt/archives/."
    
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
	--allow-unauthenticated --download-only install "${PKGS[@]}"
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
    # Save our updated gems and pkgs in the cache for later.
    cp -a "$CHROOT/var/cache/apt/archives/." "$PKG_CACHE/." 
    cp -a "$CHROOT/var/lib/gems/1.8/cache/." "$GEM_CACHE/."
    sync

    debug "Cleaning up mounts"
    # umount all the stuff we have mounted for the chroot.
    while grep -q "$CHROOT" /proc/self/mounts; do 
	for m in "${TO_UMOUNT[@]}"; do sudo umount "$m"; sleep 1; done
	sudo umount "$CHROOT"
    done
}

copy_pkgs() {
    # $1 = pool directory to build initial list of excludes against
    # $2 = directory to copy from
    # $3 = directory to copy to.
    
    # First, a couple of hashes to hold deb => revision values
    declare -A deb_pool
    declare -A dest_pool
    
    local debname=''
    local debarr=()
    local pkgs_to_copy=()
    
    
    # Scan through our pool to find pkgs we can easily omit.
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
		    pkgs_to_copy[$((${#pkgs_to_copy[@]} - 1))]="$deb"
		    dest_pool["$debname"]="$debver"
		fi
	    # Second check and see if it is already inthe install pool.
	    # If it is and it is the same or lesser version, don't copy it
	    elif [[ ${deb_pool["$debname"]} ]]; then
		if [[ ${deb_pool["$debname"]} < $debver ]]; then
		    pkgs_to_copy+=("$deb")
		else
		    debug "${debname}_${debver} in CD pool, omitting"
		fi
	    else
		# It is not already in deb_pool or dest_pool, copy it.
		pkgs_to_copy+=("$deb")
		debug "Will copy ${debname}_${debver}"
	    fi
	done
	# Now, we have a list of pkgs to copy, so do it. 
	mkdir -p "$3"
	cp -t "$3" "${pkgs_to_copy[@]}" 
    )
}

maybe_update_cache() {
    local pkgfile deb gem pkg_type rest need_update _pwd 
    debug "Processing package lists"
    # Zero out our sources.list
    > "$BUILD_DIR/extra/sources.list"
    # Download and stash any extra files we may need
    # First, build our list of repos, ppas, pkgs, and gems
    for pkgfile in "$BUILD_DIR/extra/packages/"*.list; do
	[[ -f $pkgfile ]] || continue
	while read pkg_type rest; do
	    case $pkg_type in
		repository) 
		    echo "${rest%%#*}" >> "$BUILD_DIR/extra/sources.list";;
		ppas) PPAS+=(${rest%%#*});;
		pkgs) PKGS+=(${rest%%#*});;
		gems) GEMS+=(${rest%%#*});;
	    esac
	done <"$pkgfile"
    done
    
    _pwd=$PWD
    
    # move old debs if they exist
    if [[ -d $CACHE_DIR/$OS_CODENAME/debs ]]; then
	mv "$CACHE_DIR/$OS_CODENAME/debs"/* "$PKG_CACHE"
	rm -rf "$CACHE_DIR/$OS_CODENAME/debs"
    fi
    
    cd "$PKG_CACHE"
    # second, verify that the pkgs we need are in the cache.
    for deb in "${PKGS[@]}"; do
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
	( ! -d $PKG_CACHE ) || $* =~ update-cache ]]; then
	update_caches
    else
	return 0
    fi
}

reindex_packages() (
    # Make our new packages repository.
    cd "$BUILD_DIR/extra" 
	debug "Recreating Packages.gz"
	dpkg-scanpackages pkgs /dev/null 2>/dev/null |gzip -9 >Packages.gz
)

final_build_fixups() {
    # Copy our isolinux and preseed files.
    cp -r "$BUILD_DIR/extra/isolinux"/* "$BUILD_DIR/isolinux"
    cp -r "$BUILD_DIR/extra/preseed"/* "$BUILD_DIR/preseed"
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
    
}

for cmd in sudo chroot debootstrap mkisofs dpkg-scanpackages; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done
