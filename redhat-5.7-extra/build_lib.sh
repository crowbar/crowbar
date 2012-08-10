#!/bin/bash
# This is sourced by build_crowbar.sh to enable it to stage Crowbar onto
# RHEL 5.7


# OS information for the OS we are building crowbar on to.
OS=redhat
OS_VERSION=5.7

# If we need to make a chroot to stage packages into, this is the minimal
# set of packages needed to bootstrap yum.
# This package list has only been tested on RHEL 5.7.
OS_BASIC_PACKAGES=(MAKEDEV SysVinit audit-libs basesystem bash beecrypt \
    bzip2-libs coreutils redhat-release cracklib cracklib-dicts db4 \
    device-mapper e2fsprogs elfutils-libelf e2fsprogs-libs ethtool expat \
    filesystem findutils gawk gdbm glib2 glibc glibc-common grep info \
    initscripts iproute iputils krb5-libs libacl libattr libcap libgcc libidn \
    libselinux libsepol libstdc++ libsysfs libtermcap libxml2 libxml2-python \
    mcstrans mingetty mktemp module-init-tools ncurses neon net-tools nss \
    nspr openssl pam pcre popt procps psmisc python python-libs \
    python-elementtree python-sqlite python-urlgrabber python-iniparse \
    readline rpm rpm-libs rpm-python sed setup shadow-utils sqlite sysklogd \
    termcap tzdata udev util-linux wget yum yum-metadata-parser zlib)

OS_REPO_POOL=Server
# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="RHEL5.7-Server-20110711.5-x86_64-DVD.iso"
[[ $PRIORITIES_HTTP ]] || PRIORITIES_HTTP="http://mirror.centos.org/centos/5/extras/i386/RPMS/yum-priorities-1.1.16-13.el5.centos.noarch.rpm"
[[ $PRIORITIES_RPM ]] || PRIORITIES_RPM="yum-priorities-1.1.16-13.el5.centos.noarch.rpm"

# The location of OS packages on $ISO
find_cd_pool() ( echo "$IMAGE_DIR/Server" )

# There is no public location to fetch the RHEL .iso from.  If you have one,
# you can change this function.
fetch_os_iso() {
    die "build_crowbar.sh does not know how to automatically download $ISO"
}

# Throw away packages we will not need on the
shrink_iso() {
    # Do nothing if we do not have a minimal-install set for this OS.
    [[ -f $CROWBAR_DIR/$OS_TOKEN-extra/minimal-install ]] || \
        return 0
    local pkgname pkgver
    while read pkgname pkgver; do
        INSTALLED_PKGS["$pkgname"]="$pkgver"
    done < "$CROWBAR_DIR/$OS_TOKEN-extra/minimal-install"
    mkdir -p "$BUILD_DIR/Server"
    make_chroot
    check_all_deps $(for bc in "${BARCLAMPS[@]}"; do
        echo ${BC_PKGS[$bc]}; done)
    cp -a "$IMAGE_DIR/Server/repodata" "$BUILD_DIR/Server"
    for pkgname in "${!CD_POOL[@]}"; do
        [[ ${INSTALLED_PKGS["$pkgname"]} ]] || continue
        [[ -f ${CD_POOL["$pkgname"]} ]] || \
            die "Cannot stage $pkgname from the CD!"
        cp "${CD_POOL["$pkgname"]}" "$BUILD_DIR/Server"
    done
    sudo mount --bind "$BUILD_DIR/Server" "$CHROOT/mnt"
    in_chroot 'cd /mnt; createrepo -g repodata/comps-rhel5-server-core.xml .'
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/Server"
}
 . "$CROWBAR_DIR/redhat-common/build_lib.sh"
