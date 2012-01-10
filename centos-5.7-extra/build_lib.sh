#!/bin/bash
# This is sourced by build_crowbar.sh to enable it to stage Crowbar onto
# RHEL 5.7


# OS information for the OS we are building crowbar on to.
OS=centos
OS_VERSION=5.7
OS_TOKEN="$OS-$OS_VERSION"
PKG_TYPE="rpms"
PKG_ALLOWED_ARCHES=("x86_64" "noarch")
CHROOT_PKGDIR="var/cache/yum"
CHROOT_GEMDIR="usr/lib/ruby/gems/1.8/cache"
declare -A SEEN_RPMS

# If we need to make a chroot to stage packages into, this is the minimal
# set of packages needed to bootstrap yum.  This package list has only been tested
# on RHEL 5.7.
OS_BASIC_PACKAGES=(MAKEDEV SysVinit audit-libs basesystem bash beecrypt \
    bzip2-libs coreutils centos-release cracklib cracklib-dicts db4 \
    device-mapper e2fsprogs elfutils-libelf e2fsprogs-libs ethtool expat \
    filesystem findutils gawk gdbm glib2 glibc glibc-common grep info \
    initscripts iproute iputils krb5-libs libacl libattr libcap libgcc libidn \
    libselinux libsepol libstdc++ libsysfs libtermcap libxml2 libxml2-python \
    mcstrans mingetty mktemp module-init-tools ncurses neon net-tools nss \
    nspr openssl pam pcre popt procps psmisc python python-libs \
    python-elementtree python-sqlite python-urlgrabber python-iniparse \
    readline rpm rpm-libs rpm-python sed setup shadow-utils sqlite sysklogd \
    termcap tzdata udev util-linux wget yum yum-metadata-parser zlib)

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="CentOS-5.7-x86_64-bin-DVD-1of2.iso"
[[ $PRIORITIES_HTTP ]] || PRIORITIES_HTTP="http://mirror.centos.org/centos/5/extras/i386/RPMS/yum-priorities-1.1.16-13.el5.centos.noarch.rpm"
[[ $PRIORITIES_RPM ]] || PRIORITIES_RPM="yum-priorities-1.1.16-13.el5.centos.noarch.rpm"

# we need extglobs, so enable them.
shopt -s extglob

# The location of OS packages on $ISO
find_cd_pool() ( echo "$IMAGE_DIR/CentOS" )

# There is no public location to fetch the RHEL .iso from.  If you have one,
# you can change this function.
fetch_os_iso() {
    die "build_crowbar.sh does not know how to automatically download $ISO"
}

# Have the chroot update its package metadata
chroot_update() { in_chroot yum -y makecache; }

# Install some packages in the chroot environment.
chroot_install() { 
    if [[ $1 ]]; then
	in_chroot /usr/bin/yum -y install "$@"
    fi
    in_chroot /usr/bin/yum -y update
}

# Fetch (but do not install) packages into the chroot environment
chroot_fetch() { 
    if [[ $1 ]]; then
	in_chroot /usr/bin/yum -y --downloadonly install "$@" || :
    fi
    in_chroot /usr/bin/yum -y update
}

# Make a repository file in the chroot environment.  We use this when we get a URL
# from one of the packages files (as opposed to an RPM that contains repo info.
make_repo_file() {
    # $1 = name of repo
    # $2 = Priority
    # $3 = URL
    local repo=$(mktemp "/tmp/repo-$1-XXXX.repo")
    cat >"$repo" <<EOF
[$1]
name=Repo for $1
baseurl=$3
priority=$2
enabled=1
gpgcheck=0
EOF
    sudo cp "$repo" "$CHROOT/etc/yum.repos.d/"
    rm "$repo"
}

# Add repositories to the chroot environment.
# This also add proxy information if needed.
add_repos() {
    for repo in "$@"; do
	rtype="${repo%% *}"
	rdest="${repo#* }"
	case $rtype in
	    rpm) rdest="${rdest#* }"
		f="$(mktemp /tmp/tmp-XXXXXX.rpm)"
		curl -o "$f" "$rdest"
		sudo cp "$f" "$CHROOT/tmp"
		rm "$f"
	        in_chroot /bin/rpm -Uvh "$f";;
	    bare) make_repo_file $rdest;;
	esac
    done
    [[ $USE_PROXY = "1" ]] || return 0
    (   cd "$CHROOT"
	for f in etc/yum.repos.d/*; do
	    [[ -f "$f" ]] || continue 
	    in_chroot /bin/grep -q '^proxy=' "/$f" && continue
	    in_chroot /bin/grep -q '^baseurl=http://.*127\.0\.0\.1.*' "/$f" && \
		continue
	    in_chroot sed -i "/^name/ a\proxy=http://$PROXY_HOST:$PROXY_PORT" "$f"
	    [[ $PROXY_USER ]] && \
		in_chroot sed -i "/^proxy/ a\proxy_username=$PROXY_USER" "$f"
	    [[ $PROXY_PASSWORD ]] && \
	        in_chroot sed -i "^/proxy_username/ a\proxy_password=$PROXY_PASSWORD" "$f"
	    : ;
	done
    )
}

# Check to see if something is a valid RPM package name.
is_pkg() { [[ $1 = *.rpm ]]; }

# This function makes a functional centos chroot environment.
# We do this so that we can build an install DVD that has Crowbar staged
# on it without having to have a handy Redhat/CentOS environment to build from.
# It also makes sure that even if we do, we don't inadvertently wind up installing
# packages on the build system by mistake. 
__make_chroot() {
    postcmds=()
    # Install basic packages.
    for pkg in "${OS_BASIC_PACKAGES[@]}"; do
	for f in "$IMAGE_DIR/CentOS/$pkg"-[0-9]*+(noarch|x86_64).rpm; do
	    rpm2cpio "$f" | \
		(cd "$CHROOT"; sudo cpio --extract \
		--make-directories --no-absolute-filenames \
		--preserve-modification-time)
	done
	if [[ $pkg =~ (centos|redhat)-release ]]; then
	    sudo mkdir -p "$CHROOT/tmp"
	    sudo cp "$f" "$CHROOT/tmp/${f##*/}"
	    postcmds+=("/bin/rpm -ivh --force --nodeps /tmp/${f##*/}")
	fi
    done
    # install priorities support
    mkdir -p "$CACHE_DIR/$OS_TOKEN/pkgs"
    [[ -f $CACHE_DIR/$OS_TOKEN/pkgs/$PRIORITIES_RPM ]] || \
	curl -o "$CACHE_DIR/$OS_TOKEN/pkgs/$PRIORITIES_RPM" \
	"$PRIORITIES_HTTP"
    rpm2cpio "$CACHE_DIR/$OS_TOKEN/pkgs/$PRIORITIES_RPM" | \
	( cd "$CHROOT"; sudo cpio --extract \
		--make-directories --no-absolute-filenames \
		--preserve-modification-time)
    # fix up the chroot to make sure we can use it
    sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"
    sudo rm -f "$CHROOT/etc/yum.repos.d/"*

    # Fire up Webrick to allow us to pull packages from the local CD.
    (   cd "$IMAGE_DIR"
	exec ruby -rwebrick -e \
	    "WEBrick::HTTPServer.new(:BindAddress=>\"127.0.0.1\",:Port=>54321,:DocumentRoot=>\".\").start" &>/dev/null ) &
    webrick_pid=$!
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
    in_chroot /bin/bash -c "echo 'exclude = *.i?86' >>/etc/yum.conf"

    [[ $USE_PROXY = "1" ]] && (   
	cd "$CHROOT"
	for f in etc/yum.repos.d/*; do
	    [[ -f "$f" ]] || continue 
	    in_chroot /bin/grep -q '^proxy=' "/$f" && continue
	    in_chroot /bin/grep -q '^baseurl=http://.*127\.0\.0\.1.*' "/$f" && \
		continue
	    in_chroot sed -i "/^name/ a\proxy=http://$PROXY_HOST:$PROXY_PORT" "$f"
	    [[ $PROXY_USER ]] && \
		in_chroot sed -i "/^proxy/ a\proxy_username=$PROXY_USER" "$f"
	    [[ $PROXY_PASSWORD ]] && \
	        in_chroot sed -i "^/proxy_username/ a\proxy_password=$PROXY_PASSWORD" "$f"
	    : ;
	done
    )
    # Make sure that the bootstrap only uses local data off the install DVD.
    in_chroot mv /etc/yum.repos.d /etc/yum.repos.d.old
    in_chroot mkdir -p /etc/yum.repos.d/
    make_repo_file redhat-base 99 "http://127.0.0.1:54321/"
    # have yum bootstrap everything else into usefulness
    in_chroot yum -y install yum yum-downloadonly createrepo
    # Once we have the chroot bootstrapped, restore the CentOS repos.
    # If we are using a proxy, fastestmirror usually does the Wrong Thing.
    [[ $USE_PROXY = 1 ]] && \
	in_chroot sed -ie '/^enabled/ s/1/0/' \
	/etc/yum/pluginconf.d/fastestmirror.conf
    in_chroot rm -rf /etc/yum.repos.d
    in_chroot mv /etc/yum.repos.d.old /etc/yum.repos.d
    make_repo_file redhat-base 99 "http://127.0.0.1:54321/"
}

# Extract version information from an RPM file
rpmver() {
    [[ -f $1 && $1 = *.rpm ]] || die "$1 is not an rpm!"
    if [[ ! ${SEEN_RPMS["${1##*/}"]} ]]; then
	local ver=$(rpm --queryformat \
            '%{NAME}-%{ARCH} %{EPOCH}:%{VERSION}-%{RELEASE}' \
	    --nodigest --nosignature -qp "$1")
	SEEN_RPMS["${1##*/}"]="${ver// (none):/ }"
    fi
    echo "${SEEN_RPMS["${1##*/}"]}"
}

# Get a package name in the form of $name-$arch
pkg_name() {
    local res="$(rpmver "$1")"
    echo "${res% *}"
}

# Check to see if package $1 is more recent than package $2
pkg_cmp() {
    # $1 = RPM 1
    # $2 = RPM 2
    local rpm1=$(rpmver "$1") rpm2="$(rpmver "$2")"
    [[ ${rpm1% *} = ${rpm2% *} ]] || \
	die "$1 and $2 do not reference the same package!"
    vercmp "${rpm1#* }" "${rpm2#* }"
}

# Copy our isolinux bits into place and append our kickstarts into the initrds.
final_build_fixups() {
    # Copy our isolinux files.
    debug "Updating the isolinux boot information"
    mv "$BUILD_DIR/extra/isolinux" "$BUILD_DIR"
    # Add our kickstart files into the initrd images.
    debug "Adding our kickstarts to the initrd images."
    (cd "$IMAGE_DIR"; find -name initrd.img |cpio -o) | \
	(cd "$BUILD_DIR"; cpio -i --make-directories)
    chmod -R u+rw "$BUILD_DIR"
    (cd "$BUILD_DIR/extra"; find . -name '*.ks' | \
	cpio --create --format=newc 2>/dev/null | \
		gzip -9 > "initrd.img.append"
	cat "initrd.img.append" >> "../isolinux/initrd.img"
	cat "initrd.img.append" >> "../images/pxeboot/initrd.img")
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
    mkdir -p "$BUILD_DIR/CentOS"
    cp -a "$IMAGE_DIR/repodata" "$BUILD_DIR"
    for pkgname in "${!CD_POOL[@]}"; do
	[[ ${INSTALLED_PKGS["$pkgname"]} ]] || continue
	[[ -f ${CD_POOL["$pkgname"]} ]] || \
	    die "Cannot stage $pkgname from the CD!"
	cp "${CD_POOL["$pkgname"]}" "$BUILD_DIR/CentOS"
    done
    make_chroot
    sudo mount --bind "$BUILD_DIR" "$CHROOT/mnt"
    in_chroot /bin/bash -c 'cd /mnt; createrepo -g /mnt/repodata/comps.xml -o /mnt/repodata CentOS'
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/CentOS"
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/repodata"
}

generate_minimal_install() { : ; }

# Check to make sure our required commands are installed.
for cmd in rpm; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done
