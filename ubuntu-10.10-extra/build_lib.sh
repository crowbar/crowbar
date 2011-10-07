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

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="ubuntu-$OS_VERSION-server-amd64.iso"

find_cd_pool() ( echo "$IMAGE_DIR/pool"; )

fetch_os_iso() {
    # Try and download our ISO if we don't already have it
    echo "$(date '+%F %T %z'): Downloading and caching $ISO"
    curl -o "$ISO_LIBRARY/$ISO" \
	"$ISO_MIRROR/ubuntu-iso/CDs/$OS_VERSION/$ISO" || \
	die 1 "Missing our source image"
}

chroot_update() { in_chroot /usr/bin/apt-get -y --force-yes \
    --allow-unauthenticated update; }

# Install some packages in the chroot environment.
chroot_install() { in_chroot /usr/bin/apt-get -y --force-yes \
    --allow-unauthenticated install "$@"; }

# Fetch (but do not install) packages into the chroot environment
chroot_fetch() { in_chroot /usr/bin/apt-get -y --force-yes \
    --allow-unauthenticated --download-only install "$@"; }

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

is_pkg() { [[ $1 = *.deb ]]; }

pkg_name() (
    IFS=' '
    local n="${1##*/}"
    n="${n%.deb}"
    local a=(${n//_/ })
    echo "${a[0]}-${a[2]}"
)    

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
    in_chroot /bin/bash -l

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

pkg_cmp() {
    # $1 = Debian package 1
    # $2 = Debian package 2
    local deb1=(${1//_/ }) deb2=(${2//_/ })
    [[ ${deb1[0]##*/} = ${deb2[0]##*/} ]] || \
	die "$1 and $2 do not reference the same package!"
    vercmp "${deb1[1]}" "${deb2[1]}"
}

for cmd in sudo chroot debootstrap mkisofs; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done
