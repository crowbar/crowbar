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

in_chroot() { sudo -H /usr/sbin/chroot "$CHROOT" "$@"; }
chroot_install() { in_chroot /usr/bin/yum -y install "$@"; }
chroot_fetch() { in_chroot /usr/bin/yum -y --downloadonly install "$@"; }

make_repo_file() {
    # $1 = name of repo
    # $2 = URL
    local repo=$(mktemp "/tmp/repo-$1-XXXX.repo")
    cat >"$repo" <<EOF
[$1]
name=Repo for $1
baseurl=$2
enabled=1
gpgcheck=0
EOF
    sudo cp "$repo" "$CHROOT/etc/yum.repos.d/"
}

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
    sudo rm -f "$CHROOT/etc/yum.repos.d/"*
    make_repo_file redhat-base "http://127.0.0.1:54321/Server/"
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
    in_chroot mkdir -p "/usr/lib/python2.4/site-packages/urlgrabber.broke"
    for f in "$CHROOT/usr/lib/python2.4/site-packages/urlgrabber/keepalive"*; do
	in_chroot mv "${f#$CHROOT}" \
	    "/usr/lib/python2.4/site-packages/urlgrabber.broke/"
    done
    # Make sure yum does not throw away our caches for any reason.
    in_chroot /bin/sed -i -e '/keepcache/ s/0/1/' /etc/yum.conf
    in_chroot /bin/bash -c "echo 'exclude = *.i386' >>/etc/yum.conf"
    # fourth, have yum bootstrap everything else into usefulness
    chroot_install yum yum-downloadonly
)

update_caches() {
    (   cd "$BUILD_DIR"
	exec ruby -rwebrick -e \
	    'WEBrick::HTTPServer.new(:BindAddress=>"127.0.0.1",:Port=>54321,:DocumentRoot=>".").start' &>/dev/null ) &
    webrick_pid=$!
    make_redhat_chroot
    # First, copy in our current packages and fix up ownership
    for d in "$PKG_CACHE"/*; do
	[[ -d $d ]] || continue
	in_chroot mkdir -p "/var/cache/yum/${d##*/}/packages"
	in_chroot chmod 777 "/var/cache/yum/${d##*/}/packages"
	sudo cp -a "$d/"* "$CHROOT/var/cache/yum/${d##*/}/packages"
    done
    in_chroot chown -R root:root "/var/cache/yum/"
    for repo in "${REPOS[@]}"; do
	rtype="${repo%% *}"
	rdest="${repo#* }"
	case $rtype in
	    rpm) in_chroot rpm -Uvh "$rdest";;
	    bare) make_repo_file $rdest;;
	esac
    done
    in_chroot mkdir -p "/usr/lib/ruby/gems/1.8/cache/"
    in_chroot chmod 777 "/usr/lib/ruby/gems/1.8/cache/"
    cp -a "$GEM_CACHE/." "$CHROOT/usr/lib/ruby/gems/1.8/cache/."
    # Fix up the passenger repo
    if [[ -f $CHROOT/etc/yum.repos.d/passenger.repo ]]; then
	in_chroot sed -i -e 's/\$releasever/5/g' /etc/yum.repos.d/passenger.repo
    fi
    # Second, pull down packages
    chroot_fetch "${PKGS[@]}"
    # Pull our updated packages back out.
    for d in "$CHROOT/var/cache/yum/"*"/packages"; do 
	t="${d##*yum/}"
	t="${t%/packages}"
	mkdir -p "$PKG_CACHE/$t"
	cp -a "${d}/"* "$PKG_CACHE/$t" 
    done
    chroot_install rubygems ruby-devel make
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
    cp -a "$CHROOT/usr/lib/ruby/gems/1.8/cache/." "$GEM_CACHE/."
    kill -9 $webrick_pid
    while read dev fs type opts rest; do
	sudo umount "$fs"
    done < <(tac /proc/self/mounts |grep "$CHROOT")
}

copy_pkgs() {
    # $1 = pool directory to build initial list of excludes against
    # $2 = directory to copy from
    # $3 = directory to copy to.
    mkdir -p "$3"
    cp -r -t "$3" "$2"/*
}

maybe_update_cache() {
    local pkgfile deb rpm pkg_type rest need_update _pwd 
    debug "Processing package lists"
    # Download and stash any extra files we may need
    # First, build our list of repos, ppas, pkgs, and gems
    REPOS=()
    for pkgfile in "$BUILD_DIR/extra/packages/"*.list; do
	[[ -f $pkgfile ]] || continue
	while read pkg_type rest; do
	    case $pkg_type in
		repository) REPOS+=("$rest");; 
		pkgs) PKGS+=(${rest%%#*});;
		gems) GEMS+=(${rest%%#*});;
	    esac
	done <"$pkgfile"
    done

    # Check and see if we need to update
    for rpm in "${PKGS[@]}"; do
	if [[ $(find "$PKG_CACHE" -name "$rpm*.rpm") || \
	    $(find "$BUILD_DIR/Server/" -name "$rpm*.rpm") ]]; then
	    continue
	fi
	need_update=true
	break
    done

    for gem in "${GEMS[@]}"; do
	[[ ! $(find "$GEM_CACHE" -name "$gem*.gem") ]] || continue
	need_update=true
	break
    done

     if [[ $need_update = true || \
	( ! -d $PKG_CACHE ) || $* =~ update-cache ]]; then
	update_caches
    else
	return 0
    fi
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
	cpio --create --format=newc 2>/dev/null | \
		gzip -9 >> "initrd.img.append"
	cat "initrd.img.append" >> "../isolinux/initrd.img"
	cat "initrd.img.append" >> "../images/pxeboot/initrd.img")
}

for cmd in sudo chroot createrepo mkisofs; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done
