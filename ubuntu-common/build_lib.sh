#!/bin/bash
# This file is sourced by build_crowbar.sh when you want to build Crowbar
# using Ubuntu as the base OS.  It includes build routines common to all
# Ubuntu distributions (so far).

# Server to download the mirror from if we need to.
ISO_MIRROR="http://mirror.anl.gov/pub"
PKG_TYPE="debs"
PKG_ALLOWED_ARCHES=("amd64" "all")
CHROOT_PKGDIR="var/cache/apt/archives"
CHROOT_GEMDIR="var/lib/gems/1.8/cache"
OS_METADATA_PKGS="dpkg-dev"
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
    in_chroot /usr/bin/apt-get -y --force-yes \
        --allow-unauthenticated --download-only upgrade
}

# Fetch (but do not install) packages into the chroot environment
chroot_fetch() {
    if [[ $1 ]]; then
        local p
        for p in "$@"; do
            in_chroot /usr/bin/apt-get -y --force-yes \
                --allow-unauthenticated --download-only install "$p"
        done
    fi
    in_chroot /usr/bin/apt-get -y --force-yes \
        --allow-unauthenticated --download-only upgrade
}
chroot_fetch_source() {
    [[ $1 ]] || return 0
    local p
    for p in "$@"; do
        in_chroot /bin/bash -c "cd /$CHROOT_PKGDIR; /usr/bin/apt-get -y --force-yes --allow-unauthenticated --download-only source \"$p\""
    done
    in_chroot /bin/bash -c "cd /$CHROOT_PKGDIR; dpkg-scansources . 2>/dev/null |gzip -9 >Sources.gz"
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

__barclamp_pkg_metadata_needs_update() (
    cd "$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs"
    [[ -f Packages.gz ]] || return 0
    while read fname; do
        [[ $fname -nt . ]] && return 0
    done < <(find . -name '*.deb' -type f)
    return 1
)

__make_barclamp_pkg_metadata() {
    in_chroot /bin/bash -c 'cd /mnt; dpkg-scanpackages . 2>/dev/null |gzip -9 >Packages.gz'
    sudo chown -R "$(whoami)" "$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs"
    if [[ $CURRENT_CACHE_BRANCH ]]; then
        in_cache git add "barclamps/$1/$OS_TOKEN/pkgs/Packages.gz"
        in_cache git add "barclamps/$1/$OS_TOKEN/pkgs/Sources.gz"
    fi
}

add_offline_repos() {
    in_chroot rm -f /etc/apt/sources.list
    in_chroot mkdir -p /packages/base
    in_chroot mkdir -p /packages/barclamps
    local bc
    for bc in "${BARCLAMPS[@]}"; do
        [[ -f $CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/Packages.gz ]] || continue
        sudo mkdir -p "$CHROOT/packages/barclamps/$bc"
        sudo mount --bind "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs" \
            "$CHROOT/packages/barclamps/$bc"
        add_repos "deb file:///packages/barclamps/$bc /"
    done
    sudo mount --bind "$IMAGE_DIR" "$CHROOT/packages/base"
    add_repos "deb file:///packages/base $OS_CODENAME main restricted"
}

# OS specific part of making our chroot environment.
__make_chroot() {
    # debootstrap a minimal install of our target version of
    # Ubuntu to ensure that we don't interfere with the host's package cache.
    local d repo bc f
    sudo debootstrap "$OS_CODENAME" "$CHROOT" \
        "file://$IMAGE_DIR" || \
        die 1 "Could not bootstrap our scratch target!"
    # mount some important directories for the chroot

    for d in /proc /sys /dev /dev/pts /dev/shm; do
        [[ -L $d ]] && d="$(readlink -f "$d")"
        mkdir -p "${CHROOT}$d"
        sudo mount --bind "$d" "${CHROOT}$d"
    done
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
    debug "Fixing up initrds"
    [[ -d $BUILD_DIR/initrd ]] && rm -rf initrd
    mkdir -p "$BUILD_DIR/initrd"
    # Grab _all_ the nic drivers. We probably don't need them,
    # but a little paranoia never hurt anyone.
    (   cd "$BUILD_DIR/initrd";
        debug "Adding all nic drivers"
        for udeb in "$IMAGE_DIR/pool/main/l/linux/"nic-*-generic-*.udeb; do
            ar x "$udeb"
            tar xzf data.tar.gz
            rm -rf debian-binary *.tar.gz
        done
        # bnx2x nic drivers require firmware images from the kernel image .deb
        ar x "$IMAGE_DIR/pool/main/l/linux/"linux-image-*-generic_*.deb
        tar xjf data.tar.bz2 --wildcards './lib/firmware/*/bnx2x/*'
        rm -rf debian-binary control.tar.gz data.tar.bz2
        # Make sure installing off a USB connected DVD will work
        debug "Adding USB connected DVD support"
        mkdir -p var/lib/dpkg/info
        cp "$CROWBAR_DIR/initrd/cdrom-detect.postinst" var/lib/dpkg/info
        debug "Enabling bootif support for debian-installer"
        mkdir -p lib/debian-installer-startup.d/
        [[ -f $CROWBAR_DIR/$OS_TO_STAGE-extra/patches/bootif ]] && {
            cp "$CROWBAR_DIR/$OS_TO_STAGE-extra/patches/bootif" \
                lib/debian-installer-startup.d/S32set-bootif
            chmod 755 "lib/debian-installer-startup.d/S32set-bootif"
        }
        for initrd in "install/initrd.gz" \
            "install/netboot/ubuntu-installer/amd64/initrd.gz"; do
            [[ -f $IMAGE_DIR/$initrd ]] || continue
            mkdir -p "$BUILD_DIR/${initrd%/*}"
            gunzip -c "$IMAGE_DIR/$initrd" >"$BUILD_DIR/initrd.tmp"
            find . | \
                cpio --format newc --owner root:root \
                -oAF "$BUILD_DIR/initrd.tmp"
            cat "$BUILD_DIR/initrd.tmp" | \
                gzip -9 > "$BUILD_DIR/$initrd"
        done
        rm "$BUILD_DIR/initrd.tmp"
    )
    # rm -rf "$BUILD_DIR/initrd"
}

# Check to make sure all our prerequisites are met.
for cmd in debootstrap ar; do
    which "$cmd" &>/dev/null || \
        die "Please install $cmd before trying to build Crowbar."
done
