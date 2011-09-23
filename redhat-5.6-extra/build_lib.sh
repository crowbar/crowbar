#!/bin/bash
# This is sourced by build_crowbar.sh to enable it to stage Crowbar onto
# RHEL 5.6


# OS information for the OS we are building crowbar on to.
OS=redhat
OS_VERSION=5.6
OS_TOKEN="$OS-$OS_VERSION"

# If we need to make a chroot to stage packages into, this is the minimal
# set of packages needed to bootstrap yum.  This package list has only been tested
# on RHEL 5.6.
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

# The name of the OS iso we are using as a base.
[[ $ISO ]] || ISO="RHEL5.6-Server-20110106.0-x86_64-DVD.iso"
[[ $PRIORITIES_HTTP ]] || PRIORITIES_HTTP="http://mirror.centos.org/centos/5/extras/i386/RPMS/yum-priorities-1.1.16-13.el5.centos.noarch.rpm"
[[ $PRIORITIES_RPM ]] || PRIORITIES_RPM="yum-priorities-1.1.16-13.el5.centos.noarch.rpm"

# we need extglobs, so enable them.
shopt -s extglob

# There is no public location to fetch the RHEL .iso from.  If you have one,
# you can change this function.
fetch_os_iso() {
    die "build_crowbar.sh does not know how to automatically download $ISO"
}

proxy_command() {
  if [ "$USE_PROXY" == "1" ] ; then
    no_proxy="localhost,localhost.localdomain,127.0.0.0/8,$PROXY_HOST" http_proxy="http://$PROXY_USER:$PROXY_PASSWORD@$PROXY_HOST:$PROXY_PORT/" https_proxy="http://$PROXY_USER:$PROXY_PASSWORD@$PROXY_HOST:$PROXY_PORT/" "$@"; 
  else
    "$@" 
  fi
}
in_chroot_use_proxy() { 
  if [ "$USE_PROXY" == "1" ] ; then
    sudo -H no_proxy="localhost,localhost.localdomain,127.0.0.0/8,$PROXY_HOST" http_proxy="http://$PROXY_USER:$PROXY_PASSWORD@$PROXY_HOST:$PROXY_PORT/" https_proxy="http://$PROXY_USER:$PROXY_PASSWORD@$PROXY_HOST:$PROXY_PORT/" /usr/sbin/chroot "$CHROOT" "$@"; 
  else
    sudo -H /usr/sbin/chroot "$CHROOT" "$@"; 
  fi
}

# Run a command in our chroot environment.
in_chroot() { sudo -H /usr/sbin/chroot "$CHROOT" "$@"; }

# Install some packages in the chroot environment.
chroot_install() { in_chroot /usr/bin/yum -y install "$@"; }

# Fetch (but do not install) packages into the chroot environment
chroot_fetch() { in_chroot /usr/bin/yum -y --downloadonly install "$@"; }

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
}

make_repo_file_proxy() {
    # $1 = name of repo
    # $2 = priority
    # $3 = URL
    if [ "$USE_PROXY" != "1" ] ; then
      make_repo_file $1 $2 $3
      return
    fi
    local repo=$(mktemp "/tmp/repo-$1-XXXX.repo")
    cat >"$repo" <<EOF
[$1]
name=Repo for $1
baseurl=$3
priority=$2
enabled=1
gpgcheck=0
proxy=http://$PROXY_HOST:$PROXY_PORT
proxy_username=$PROXY_USER
proxy_password=$PROXY_PASSWORD
EOF
    sudo cp "$repo" "$CHROOT/etc/yum.repos.d/"
}


# This function makes a functional redhat chroot environment.
# We do this so that we can build an install DVD that has Crowbar staged
# on it without having to have a handy Redhat/CentOS environment to build from.
# It also makes sure that even if we do, we don't inadvertently wind up installing
# packages on the build system by mistake. 
make_redhat_chroot() (
    postcmds=()
    mkdir -p "$CHROOT"
    cd "$IMAGE_DIR/Server"
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
    if [ ! -f "$ISO_LIBRARY/$PRIORITIES_RPM" ] ; then
      cd "$ISO_LIBRARY"
      proxy_command wget -q -O "$ISO_LIBRARY/$PRIORITIES_RPM" "$PRIORITIES_HTTP"
      cd -
    fi
    if [ -f "$ISO_LIBRARY/$PRIORITIES_RPM" ] ; then
      cd "$ISO_LIBRARY"
      rpm2cpio "$ISO_LIBRARY/$PRIORITIES_RPM" | (cd "$CHROOT"; sudo cpio --extract \
		--make-directories --no-absolute-filenames \
		--preserve-modification-time)
      cd -
    fi
    # second, fix up the chroot to make sure we can use it
    sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"
    sudo rm -f "$CHROOT/etc/yum.repos.d/"*
    make_repo_file redhat-base 99 "http://127.0.0.1:54321/Server/"
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


fix_repo_proxy() {
  if [ "$USE_PROXY" == "1" ] ; then
    for f in `ls $CHROOT/etc/yum.repos.d/* | grep -v redhat-base`; do
      sudo sed -i "/^name/ a\proxy=http://$PROXY_HOST:$PROXY_PORT\nproxy_username=$PROXY_ESC_USER\nproxy_password=$PROXY_PASSWORD" $f
    done
  fi
}

# This function does all the actual work of updating the caches once we have a
# package fetching chroot environment.
update_caches() {
    # Fire up Webrick to act as a local webserver for the chroot
    # to install packages from.
    (   cd "$IMAGE_DIR"
	exec ruby -rwebrick -e \
	    "WEBrick::HTTPServer.new(:BindAddress=>\"127.0.0.1\",:Port=>54321,:DocumentRoot=>\".\").start" &>/dev/null ) &
    webrick_pid=$!
    make_redhat_chroot

    # set up repos for any other packages we want to grab.
    for repo in "${REPOS[@]}"; do
	rtype="${repo%% *}"
	rdest="${repo#* }"
	case $rtype in
	    rpm) rdest="${rdest#* }"
                 in_chroot_use_proxy wget -q -O /tmp/rpm.rpm "$rdest" 
	         in_chroot rpm -Uvh /tmp/rpm.rpm;;
	    bare) make_repo_file_proxy $rdest;;
	esac
    done


    # Copy in our current cached packages and fix up ownership.
    # This prevents excessive downloads.
    for d in "$PKG_CACHE"/*; do
	[[ -d $d ]] || continue
	in_chroot mkdir -p "/var/cache/yum/${d##*/}/packages"
	in_chroot chmod 777 "/var/cache/yum/${d##*/}/packages"
	sudo cp -a "$d/"* "$CHROOT/var/cache/yum/${d##*/}/packages"
    done
    in_chroot chown -R root:root "/var/cache/yum/"
    
    fix_repo_proxy
    # Copy in our gems.
    in_chroot mkdir -p "/usr/lib/ruby/gems/1.8/cache/"
    in_chroot chmod 777 "/usr/lib/ruby/gems/1.8/cache/"
    cp -a "$GEM_CACHE/." "$CHROOT/usr/lib/ruby/gems/1.8/cache/."
     # Fetch any packages that yum thinks is missing.
    chroot_fetch "${PKGS[@]}"
    # Copy our new packages back out of the cache.
    for d in "$CHROOT/var/cache/yum/"*; do
	[[ -d $d/packages ]] || continue
	t="${d##*/}"
	mkdir -p "$PKG_CACHE/$t"
	cp -a "${d}/packages/." "$PKG_CACHE/$t/." 
    done
    # Install prerequisites for building gems.  We need all of them
    # to make sure we get all of the gems we want.
    chroot_install rubygems ruby-devel make gcc kernel-devel curl-devel sqlite-devel mysql-devel
    debug "Fetching Gems"
    echo "There may be build failures here, we can safely ignore them."
    for gem in "${GEMS[@]}"; do
	# If we were asked for an exact gem version, get it.
	# Otherwise, gem will grab the latest version.
	gemname=${gem%%-[0-9]*}
	gemver=${gem#$gemname-}
	gemopts=(install --no-ri --no-rdoc)
	[[ $gemver && $gemver != $gem ]] && gemopts+=(--version "= ${gemver}")
	in_chroot_use_proxy /usr/bin/gem "${gemopts[@]}" "$gemname"
    done
    # Copy our updated gem cache back out of the chroot.
    cp -a "$CHROOT/usr/lib/ruby/gems/1.8/cache/." "$GEM_CACHE/."
    # Tear the chroot down.
    kill -9 $webrick_pid
    while read dev fs type opts rest; do
	sudo umount "$fs"
    done < <(tac /proc/self/mounts |grep "$CHROOT")
    [[ -d $CHROOT ]] && sudo -H rm -rf "$CHROOT"
}

# A couple of utility functions for comparing version numbers.
num_re='^[0-9]+$'
__cmp() {
    [[ $1 || $2 ]] || return 255 # neither 1 nor 2 or set, we are done
    [[ $1 && ! $2 ]] && return 2 # 1 is set and 2 is not, 1 > 2
    [[ ! $1 && $2 ]] && return 0 # 2 is set and 1 is not, 1 < 2
    local a="$1" b="$2"
    if [[ $a =~ $num_re && $b =~ $num_re ]]; then #both numbers, numeric cmp.
	# make sure leading zeros do not confuse us
	a=${a##0} b=${b##0}
	((${a:=0} > ${b:=0})) && return 2
	(($a < $b)) && return 0
	return 1
    else # string compare
	[[ $a > $b ]] && return 2
	[[ $a < $b ]] && return 0
	return 1
    fi
}

vercmp(){
    # $1 = version string of first package
    # $2 = version string of second package
    local ver1=()
    local ver2=()
    local i=0
    IFS='.-_ +' read -rs -a ver1 <<< "$1"
    IFS='.-_ +' read -rs -a ver2 <<< "$2"
    for ((i=0;;i++)); do
	__cmp "${ver1[$i]}" "${ver2[$i]}"
	case $? in
	    2) return 0;;
	    0) return 1;;
	    255) return 1;;
	esac
    done
}

# Extract version information from an RPM file
rpmver() { 
    rpm --queryformat '%{NAME}-%{ARCH} %{VERSION}-%{RELEASE}' \
	--nodigest --nosignature -qp "$1"
}

# Copy packages into their final destinations.  This function
# take care to not duplicate packages that are already on the install 
# media, or that we already have a later version of.
copy_pkgs() {
    # $1 = pool directory to build initial list of excludes against
    # $2 = directory to copy from
    # $3 = directory to copy to.
    declare -A pool_pkgs
    declare -A dest_pool
    local pkgname=  pkgver=
    local pkgs_to_copy=()
    mkdir -p "$3"
    # Build a list of RPM packages already on the install media.
    # We will skip copying them.
    while read pkg; do
	[[ -f $pkg && $pkg = *.rpm ]] || continue
	read pkgname pkgver <<< \
	    $(rpmver "$pkg")
	[[ $pkgname = *-i?86 ]] && continue 
	pool_pkgs["$pkgname"]="$pkgver"
    done < <(find "$1/Server" -name '*.rpm')
    (   cd "$2"
	declare -A target_dirs
	while read f; do
	    # Skip all non-RPM files.
	    [[ -f $f && $f = *.rpm ]] || continue
	    # Skip all .src.rpms
	    [[ $f = *.src.rpm ]] && continue
	    # Get version information from the RPM.
	    read pkgname pkgver <<< $(rpmver "$f")
	    # Skip 32 bit RPM files.
	    [[ $pkgname = *-i?86 ]] && continue
	    
	    # Test to see if this package is in a directory we have not seen before.
	    # If it is, save that directory for later.
	    pkgdir=${f%/*}
	    [[ ${target_dirs["pkgdir"]} ]] || target_dirs["$pkgdir"]=$pkgdir
	    if [[ ${dest_pool["$pkgname"]} ]]; then
		# We have seen another instance of this package before, and it
		# is not on the install media.
		if vercmp "$pkgver" "${dest_pool["$pkgname"]}"; then
		    # This version is newer than the one we have seen, overwrite the
		    # other one.
		    pkgs_to_copy[$((${#pkgs_to_copy[@]} - 1))]="$f"
		    dest_pool["$pkgname"]="$pkgver"
		fi
	    elif [[ ${pool_pkgs["${pkgname}"]} ]]; then
		# This package also exists on the install media.
		if vercmp "$pkgver" "${pool_pkgs["${pkgname##*/}"]}"; then
		    # but this one is newer.  Arrange for it to be copied.
		    pkgs_to_copy+=("$f")
		    dest_pool["$pkgname"]="$pkgver"
		fi
	    else
		# We have not seen this package before.  Copy it.
		pkgs_to_copy+=("$f")
		dest_pool["$pkgname"]="$pkgver"
	    fi
	    # Make sure we sort by package name, not by leading directory tree.
	done < <(find . -name '*.rpm' |sort -t / -k 3)
	# Make all the directories we captured earlier.
	for d in "${target_dirs[@]}"; do
	    mkdir -p "$3/$d"
	done
	# Copy only what we want.
	cp -r -t "$3" "${pkgs_to_copy[@]}"
    )
}

# This function checks to see if we need or asked for a cache update, and performs
# one if we do.
maybe_update_cache() {
    local pkgfile deb rpm pkg_type rest _pwd l t
    debug "Processing package lists"
    # Download and stash any extra files we may need
    # First, build our list of repos, ppas, pkgs, and gems
    REPOS=()

    for bc in "${BARCLAMPS[@]}"; do
	yml="$CROWBAR_DIR/barclamps/$bc/crowbar.yml"
	[[ -f $yml ]] || continue
	echo "Processing $yml"
	for t in repos raw_pkgs pkgs; do
	    while read l; do
		echo "Found $t $l"
		case $t in
		    repos) REPOS+=("$l");;
		    pkgs) PKGS+=("$l");;
		    raw_pkgs) [[ -f $PKG_CACHE/raw_downloads/${l##*/} ]] && continue
			mkdir -p "$PKG_CACHE/raw_downloads"
			curl -s -S -o "$PKG_CACHE/raw_downloads/${l##*/}" "$l";;
		esac
	    done < <("$CROWBAR_DIR/parse_yml.rb" "$yml" rpms "$t" 2>/dev/null)
	done
	while read l; do
	    GEMS+=("$l")
	done < <("$CROWBAR_DIR/parse_yml.rb" "$yml" gems pkgs 2>/dev/null)
	# l = the full URL to download
	# t = the location on the ISO it should wind up in
	while read l t; do
	    case $t in
	        # If we were not given a destination, just stick it in the file cache
		'') t="$FILE_CACHE";;
		# Anything else will wind up in a directory under $FILE_CACHE
		*) mkdir -p "$FILE_CACHE/$t"
		    t="$FILE_CACHE/$t";;
	    esac
	    [[ -f $t/${l##*/} ]] || wget -q --continue "$l" -O "$t/${l##*/}"
	done < <("$CROWBAR_DIR/parse_yml.rb" "$yml" extra_files 2>/dev/null)

    done

    for pkgfile in "$BUILD_DIR/extra/packages/"*.list; do
	[[ -f $pkgfile ]] || continue
	while read pkg_type rest; do
	    if [[ $pkg_type = repository ]]; then
		REPOS+=("${rest%%#*}")
	    else
		for r in ${rest%%#*}; do
		    case $pkg_type in
			pkgs) is_in $r "${PKGS[@]}" && echo "$t needs to be removed from the .list file it is in." >&2; PKGS+=($r);;
			gems) is_in $r "${GEMS[@]}" && echo "$t needs to be removed from the .list file it is in." >&2; GEMS+=($r);;
		    esac
		done
	    fi
	done <"$pkgfile"
    done

    # Check and see if we need to update
    for rpm in "${PKGS[@]}"; do
	if [[ $(find "$PKG_CACHE" "$IMAGE_DIR/Server/" -name "$rpm*.rpm") ]]; then
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

     if [[ $need_update = true ]]; then
	update_caches
    else
	return 0
    fi
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

# Check to make sure our required commands are installed.
for cmd in sudo chroot createrepo mkisofs rpm; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done
