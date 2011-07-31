#!/bin/bash
# This is sourced by build_crowbar.sh to enable it to stage Crowbar onto
# RHEL 5.6


# OS information for the OS we are building openstack on to.
OS=redhat
OS_VERSION=5.6
OS_TOKEN="$OS-$OS_VERSION"
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
    termcap tzdata udev util-linux yum yum-metadata-parser zlib)

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="RHEL5.6-Server-20110106.0-x86_64-DVD.iso"

# we need extglobs, so enable them.
shopt -s extglob

fetch_os_iso() {
    die "build_crowbar.sh does not know how to automatically download $ISO"
}

in_chroot() { sudo /usr/sbin/chroot "$CHROOT" "$@"; }

make_redhat_chroot() (
    
    postcmds=()
    mkdir -p "$CHROOT"
    sudo mount -t tmpfs -osize=2G none "$CHROOT"
    cd "$BUILD_DIR/Server"
    # first, extract our core files into the chroot.
    for pkg in "${OS_BASIC_PACKAGES[@]}"; do
	for f in "$pkg"-[0-9]*+(noarch|x86_64).rpm; do
	    rpm2cpio "$f" | (cd "$CHROOT"; sudo cpio --extract \
		--make-directories --no-absolute-filenames \
		--preserve-modification-time)
	done
	if [[ $pkg =~ (centos|redhat)-release ]]; then
	    mkdir -p "$CHROOT/tmp"
	    cp "$f" "$CHROOT/tmp/$f"
	    postcmds+=("/bin/rpm -ivh --force --nodeps /tmp/$f")
	fi
    done
    # second, fix up the chroot to make sure we can use it
    sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"
    repo=$(mktemp /tmp/crowbar-repo-XXXXXXXX)
    cat >"$repo" <<EOF
[redhat-base]
name=Redhat Base Repo]
baseurl=http://127.0.0.1:54321/Server/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-redhat-release
EOF
    sudo rm -f "$CHROOT/etc/yum.repos.d/"*
    sudo cp "$repo" "$CHROOT/etc/yum.repos.d"
    for d in proc sys dev dev/pts; do
	mkdir -p "$CHROOT/$d"
	sudo mount --bind "/$d" "$CHROOT/$d"
    done
    # third, run any post cmds we got earlier
    for cmd in "${postcmds[@]}"; do
	in_chroot $cmd
    done
    # Work around packages we don't have, but that the yum bootstrap
    # will grab for us.
    sudo mkdir -p "$CHROOT/usr/lib/python2.4/site-packages/urlgrabber.broke"
    for f in "$CHROOT/usr/lib/python2.4/site-packages/urlgrabber/keepalive/"*; do
	sudo mv "$f" "$CHROOT/usr/lib/python2.4/site-packages/urlgrabber.broke/"
    done
    # fourth, have yum bootstrap everything else into usefulness
    in_chroot /usr/bin/yum -y install yum

    # last, install the yum-downloadonly plugin.
    in_chroot /usr/bin/yum -y install yum-downloadonly
)

update_caches() {
    # First, make our chroot and mount it.
    # Second, make our chroot environment
    
    in_chroot /bin/bash -l
}

copy_pkgs() {
    # $1 = pool directory to build initial list of excludes against
    # $2 = directory to copy from
    # $3 = directory to copy to.
    mkdir -p "$3"
    cp -t "$3" "$2"/*
}

maybe_update_cache() {
    local pkgfile deb rpm pkg_type rest need_update _pwd 
    debug "Processing package lists"
    make_redhat_chroot
    
    # Zero out our sources.list
    > "$BUILD_DIR/extra/sources.list"
    # Download and stash any extra files we may need
    # First, build our list of repos, ppas, pkgs, and gems
    for pkgfile in "$BUILD_DIR/extra/packages/"*.list; do
	[[ -f $pkgfile ]] || continue
	while read pkg_type rest; do
	    case $pkg_type in
		repository) in_chroot /bin/$rest;; 
		pkgs) PKGS+=(${rest%%#*});;
		gems) GEMS+=(${rest%%#*});;
	    esac
	done <"$pkgfile"
    done
    update_caches
    while read dev fs type opts rest; do
	sudo umount "$fs"
    done < <(tac /proc/self/mounts |grep "$CHROOT")
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
