#!/bin/bash

#
# This will need access to yum repositories.
# Make sure your yum system is setup to work.  
# To run as non-root, you will need an sudoer entry that looks like this:
# user  ALL=(ALL)       NOPASSWD: /path/to/build_sledgehammer.sh
#
# Also SELinux needs to be turned off
#

die() { local _r=$1; shift; echo "$@"; exit $1; }

mkdir -p cache

/usr/bin/livecd-creator --config=centos-sledgehammer.ks --cache=./cache -f sledgehammer || die  "Couldn't build full iso image"
rm -fr tftpboot
/usr/bin/livecd-iso-to-pxeboot sledgehammer.iso || \
    die "Could not generate PXE boot information from Sledgehammer"
rm sledgehammer.iso

mkdir -p bin || die -2 "Couldn't make bin directory"
tar czf bin/sledgehammer-tftpboot.tar.gz tftpboot

chmod -R ugo+w bin
rm -rf tftpboot
exit 0
