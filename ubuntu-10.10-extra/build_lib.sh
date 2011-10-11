#!/bin/bash
# This file is sourced by build_crowbar.sh when you want to build Crowbar 
# using Ubuntu 10.10 as the base OS.  It includes all Ubuntu 10.10 specific
# build routines.

# OS information for the OS we are building crowbar on to.
OS=ubuntu
OS_VERSION=10.10
OS_TOKEN="$OS-$OS_VERSION"
OS_CODENAME=maverick
# Server to download the mirror from if we need to.
ISO_MIRROR="http://mirror.anl.gov/pub"
PKG_TYPE="debs"
PKG_ALLOWED_ARCHES=("amd64" "all")
CHROOT_PKGDIR="var/cache/apt/archives"
CHROOT_GEMDIR="var/lib/gems/1.8/cache"
declare -A SEEN_DEBS
# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="ubuntu-$OS_VERSION-server-amd64.iso"

# The location for OS packages on $ISO
find_cd_pool() ( echo "$IMAGE_DIR/pool"; )

fetch_os_iso() {
    # Try and download our ISO if we don't already have it
    echo "$(date '+%F %T %z'): Downloading and caching $ISO"
    curl -o "$ISO_LIBRARY/$ISO" \
	"$ISO_MIRROR/ubuntu-iso/CDs/$OS_VERSION/$ISO" || \
	die 1 "Missing our source image"
}

# Have the chroot update its package databases.
chroot_update() { in_chroot /usr/bin/apt-get -y --force-yes \
    --allow-unauthenticated update; }

# Install some packages in the chroot environment.
chroot_install() { 
    if [[ $1 ]]; then
	in_chroot /usr/bin/apt-get -y --force-yes \
	    --allow-unauthenticated install "$@"
    fi
}

# Fetch (but do not install) packages into the chroot environment
chroot_fetch() {
    if [[ $1 ]]; then
	in_chroot /usr/bin/apt-get -y --force-yes \
	    --allow-unauthenticated --download-only install "$@"
    fi
}

# Add repositories to the local chroot environment.
add_repos() {
    local repo ppas=()
    local f=$(mktemp /tmp/ubuntu_repos.XXXXXX)
    for repo in "$@"; do
	case $repo in
	    ppa*) ppas+=("${ppa#* }");;
	    deb*) echo "$repo" >> "$f";;
	    *) die "Unknown Debian repository type $repo";;
	esac
    done
    in_chroot mkdir -p /etc/apt/sources.list.d
    sudo cp "$f" "$CHROOT/etc/apt/sources.list.d/${f##*.}.list"
    rm "$f"
    [[ $ppas ]] || return 0
    chroot_install python-software-properties 
    for repo in "${ppas[@]}"; do
	in_chroot apt-add-repository "ppa:${repo}"
    done
}

# Test to see we were passed a valid package file name.
is_pkg() { [[ $1 = *.deb ]]; }

# Look up name and version information for a package using 
# dpkg.  Make sure and memoize things.
dpkg_info() {
    # $1 = package to examine
    local name arch ver f1 f2
    [[ -f $1 && $1 = *.deb ]] || die "$1 is not a debian package!"
    if [[ ! ${SEEN_DEBS["${1##*/}"]} ]]; then
	while read f1 f2; do
	    case $f1 in
		Package:) name="$f2";;
		Version:) ver="$f2";;
		Architecture:) arch="$f2";;
	    esac
	    [[ $name && $ver && $arch ]] && break || :
	done < <(dpkg -I "$1")
	SEEN_DEBS["${1##*/}"]="$name-$arch $ver"
    fi
    echo "${SEEN_DEBS["${1##*/}"]}"
}

# Get the package file name in $name-$arch format.
pkg_name() {
    local n="$(dpkg_info "$1")"
    echo "${n%% *}"
}

# OS specific part of making our chroot environment.
__make_chroot() {
    # debootstrap a minimal install of our target version of
    # Ubuntu to ensure that we don't interfere with the host's package cache.
    local d repo bc f
    sudo mount -t tmpfs -o size=4G "$OS_TOKEN-chroot" "$CHROOT"
    sudo debootstrap "$OS_CODENAME" "$CHROOT" \
	"file://$IMAGE_DIR" || \
	die 1 "Could not bootstrap our scratch target!"
    # mount some important directories for the chroot
    for d in proc sys dev dev/pts; do
	bind_mount "/$d" "$CHROOT/$d"
    done
    in_chroot mkdir -p "/base_repo"
    sudo mount --bind "$IMAGE_DIR" "$CHROOT/base_repo"
    # make sure the chroot can resolve hostnames
    sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"
    # make sure the chroot honors proxies
    if [[ $http_proxy || $https_proxy ]]; then
	f=$(mktemp /tmp/apt.http.conf.XXXXXX)
	[[ $http_proxy ]] && echo \
	    "Acquire::http::Proxy \"$http_proxy\";" >> "$f"
	[[ $https_proxy ]] && echo \
	    "Acquire::https::Proxy \"$https_proxy\";" >> "$f"
	echo "Acquire::http::Proxy::127.0.0.1 \"DIRECT\";" >> "$f"
	in_chroot mkdir -p "/etc/apt/apt.conf.d/"
	sudo cp "$f" "$CHROOT/etc/apt/apt.conf.d/00http_proxy"
    fi
}

# Test to see of package $1 is more recent than package $2
pkg_cmp() {
    # $1 = Debian package 1
    # $2 = Debian package 2
    local deb1="$(dpkg_info "$1")"
    local deb2="$(dpkg_info "$2")"
    [[ ${deb1%% *} = ${deb2%% *} ]] || \
	die "$1 and $2 do not reference the same package!"
    vercmp "${deb1#* }" "${deb2#* }"
}

final_build_fixups() {
    # Copy our isolinux and preseed files.
    mv "$BUILD_DIR/extra/isolinux" "$BUILD_DIR/extra/preseed" "$BUILD_DIR"
    # Copy our initrd images
    (cd "$IMAGE_DIR"; find -name initrd.gz |cpio -o) | \
       (cd "$BUILD_DIR"; cpio -i --make-directories)
    chmod -R u+w "$BUILD_DIR"
    # Fix up the initrd
    (   cd "$CROWBAR_DIR/initrd"
       debug "Fixing up initrd"
       [[ -d scratch ]] && rm -rf scratch
       mkdir scratch
        # Grab _all_ the nic drivers. We probably don't need them,
        # but a little paranoia never hurt anyone.
       (   cd scratch;
           debug "Adding all nic drivers"
           for udeb in "$IMAGE_DIR/pool/main/l/linux/"nic-*-generic-*.udeb; do
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

# Check to make sure all our prerequisites are met.
for cmd in sudo chroot debootstrap mkisofs; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done
