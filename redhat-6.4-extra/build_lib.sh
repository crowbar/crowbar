#!/bin/bash
# This is sourced by build_crowbar.sh to enable it to stage Crowbar onto
# RHEL 6.4


# OS information for the OS we are building crowbar on to.
OS=redhat
OS_VERSION=6.4

# If we need to make a chroot to stage packages into, this is the minimal
# set of packages needed to bootstrap yum.  This package list has only been tested
# on RHEL 6.4.

OS_BASIC_PACKAGES=(MAKEDEV upstart audit-libs basesystem bash binutils \
    bzip2-libs chkconfig cracklib cracklib-dicts crontabs coreutils db4 \
    device-mapper e2fsprogs e2fsprogs-libs elfutils-libelf ethtool expat \
    file-libs filesystem findutils gawk gdbm glib2 glibc glibc-common grep \
    info initscripts iputils keyutils-libs krb5-libs libacl libattr libcap \
    libcom_err libgcc libidn libselinux libsepol libstdc++ libsysfs libgcrypt \
    libnih dbus-libs libcurl curl lua compat-libtermcap libutempter libxml2 \
    libxml2-python logrotate m2crypto mcstrans mingetty mlocate \
    module-init-tools ncurses ncurses-libs neon net-tools nss nss-sysinit \
    nss-softokn nss-softokn-freebl openldap libssh2 cyrus-sasl-lib nss-util \
    nspr openssl pam passwd libuser pcre popt procps psmisc python \
    python-libs python-pycurl python-iniparse python-urlgrabber readline rpm \
    rpm-libs rpm-python sed setup shadow-utils redhat-release-server-6Server \
    sqlite rsyslog tzdata udev util-linux-ng xz xz-libs yum \
    yum-plugin-downloadonly yum-metadata-parser yum-utils zlib)

#
# See:
#   http://www.steve.org.uk/Software/rinse/
#   http://repository.steve.org.uk/cgi-bin/hgwebdir.cgi/rinse/file/964ec9650f08/etc

OS_REPO_POOL=""
SHRINK_ISO=true

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="RHEL6.4-20130130.0-Server-x86_64-DVD1.iso"

# The location of OS packages on $ISO
find_cd_pool() ( echo "$IMAGE_DIR/Packages" )

# There is no public location to fetch the RHEL .iso from.  If you have one,
# you can change this function.
fetch_os_iso() {
    die "Cannot download $ISO.  Please manually install it into $ISO_LIBRARY."
}

 . "$CROWBAR_DIR/redhat-common/build_lib.sh"
