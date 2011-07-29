#!/bin/bash
# This is sourced by build_crowbar.sh to enable it to stage Crowbar onto
# RHEL 5.6


# OS information for the OS we are building openstack on to.
OS=redhat
OS_VERSION=5.6
OS_TOKEN="$OS-$OS_VERSION"

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="RHEL5.6-Server-20110106.0-x86_64-DVD.iso"

fetch_os_iso() {
    die "build_crowbar.sh does not know how to automatically download $ISO"
}

update_caches() {
    die "build_crowbar.sh does not know how to update our package cache for $OS_TOKEN"
}

copy_pkgs() {
    # $1 = pool directory to build initial list of excludes against
    # $2 = directory to copy from
    # $3 = directory to copy to.
    mkdir -p "$3"
    cp -t "$3" "$2"/*
}

maybe_update_cache() {
    # not implimented yet
    return 0
}

reindex_packages() (
    # Make our new packages repository.
    cd "$BUILD_DIR/extra/pkgs" 
    debug "Creating yum repository"
    createrepo --update -d .
)

final_build_fixups() {
    # Copy our isolinux and preseed files.
    debug "Updating the isolinux boot information"
    cp -r "$BUILD_DIR/extra/isolinux"/* "$BUILD_DIR/isolinux"
    # Add our kickstart files into the initrd images.
    debug "Adding our kickstarts to the initrd images."
    (cd "$BUILD_DIR/extra"; find . -name '*.ks' | \
	cpio --create --format=newc --owner root:root 2>/dev/null | \
		gzip -9 >> "initrd.img.append"
	cat "initrd.img.append" >> "../isolinux/initrd.img"
	cat "initrd.img.append" >> "../images/pxeboot/initrd.img")
}

for cmd in sudo chroot createrepo mkisofs; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done
