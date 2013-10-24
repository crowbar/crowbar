#!/bin/bash
# This is sourced by the OpenSUSE release specific build extras to provide common
# OpenSUSE build functionality.


# Setup other stuff we need
OS_TOKEN="$OS-$OS_VERSION"
PKG_TYPE="rpms"
PKG_ALLOWED_ARCHES=("x86_64" "noarch")
CHROOT_PKGDIR="var/cache/zypp"
CHROOT_GEMDIR="usr/lib64/ruby/gems/1.9.1/cache"
OS_METADATA_PKGS="createrepo"

declare -A SEEN_RPMS

# we need extglobs, so enable them.
shopt -s extglob

# Have the chroot update its package metadata
chroot_update() { in_chroot /usr/bin/zypper refresh; }

# Install some packages in the chroot environment.
chroot_install() {
    if [[ $1 ]]; then
        in_chroot /usr/bin/zypper install -l -f -y "$@"
    fi
    in_chroot /usr/bin/zypper update -y
}

install_rubygems() {
    in_chroot which gem &>/dev/null && return 0
    chroot_install rubygems\* ruby-devel make gcc kernel-devel
}

# Fetch (but do not install) packages into the chroot environment
chroot_fetch() {
    local p
    for p in "$@"; do
        in_chroot /usr/bin/zypper install -y --download-only "$p" || :
    done
    in_chroot /usr/bin/zypper update -y
}

# Make a repository file in the chroot environment.  We use this when we get a URL
# from one of the packages files (as opposed to an RPM that contains repo info.
make_repo_file() {
    # $1 = name of repo
    # $2 = Priority
    # $3 = URL
    [[ -f "$CHROOT/etc/zypp/repos.d/repo-$1.repo" ]] && return
    local repo=$(mktemp "/tmp/repo-$1-XXXX.repo")
    cat >"$repo" <<EOF
[$1]
name=Repo for $1
baseurl=$3
enabled=1
gpgcheck=0
path=/
type=NONE
autorefresh=1
keeppackages=1
EOF
    if [[ $RPM_PRIORITIES ]]; then
        echo "priority=$2" >>"$repo"
    fi
    sudo cp "$repo" "$CHROOT/etc/zypp/repos.d/repo-$1.repo"
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
                curl -L -o "$f" "$rdest"
                sudo cp "$f" "$CHROOT/tmp"
                rm "$f"
                in_chroot /bin/rpm -Uvh "$f";;
            bare) make_repo_file $rdest;;
        esac
    done
    [[ $USE_PROXY = "1" ]] || return 0
    (   cd "$CHROOT"
        for f in etc/zypp/repos.d/*; do
            [[ -f "$f" ]] || continue
            in_chroot /bin/grep -q "'^proxy='" "/$f" && continue
            in_chroot /bin/grep -q "'^baseurl=http://.*127\.0\.0\.1.*'" "/$f" && \
                continue
            in_chroot sed -i "'/^name/ a\proxy=http://$PROXY_HOST:$PROXY_PORT'" "$f"
            [[ $PROXY_USER ]] && \
                in_chroot sed -i "'/^proxy/ a\proxy_username=$PROXY_USER'" "$f"
            [[ $PROXY_PASSWORD ]] && \
                in_chroot sed -i "'^/proxy_username/ a\proxy_password=$PROXY_PASSWORD'" "$f"
            : ;
        done
    )
}

# Check to see if something is a valid RPM package name.
is_pkg() { [[ $1 = *.rpm ]]; }

__barclamp_pkg_metadata_needs_update() (
    cd "$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs"
    [[ -d repodata ]] || return 0
    while read fname; do
        [[ $fname -nt . ]] && return 0
    done < <(find . -name '*.rpm' -type f)
    return 1
)

__make_barclamp_pkg_metadata () {
    in_chroot "cd /mnt; createrepo -d -q ."
    sudo chown -R "$(whoami)" "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs"
    touch "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/repodata"
    if [[ $CURRENT_CACHE_BRANCH ]]; then
        CACHE_NEEDS_COMMIT=true
        in_cache git add "barclamps/$bc/$OS_TOKEN/pkgs/repodata"
    fi
}

add_offline_repos() (
    in_chroot mkdir -p /etc/zypp/repos.d
    for bc in "${BARCLAMPS[@]}"; do
        [[ -d $CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/repodata ]] || continue
        sudo mkdir -p "$CHROOT/packages/barclamps/$bc"
        sudo mount --bind "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs" \
            "$CHROOT/packages/barclamps/$bc"
        make_repo_file "barclamp-$bc" 99 "file:///packages/barclamps/$bc"
    done
)

__pkg_location() {
   echo "$(find_cd_pool)/$pkg"
}


# This function makes a functional OpenSUSE chroot environment.
# We do this so that we can build an install DVD that has Crowbar staged
# on it without having to have a handy OpenSUSE/Redhat/CentOS environment to build from.
# It also makes sure that even if we do, we don't inadvertently wind up installing
# packages on the build system by mistake.
__make_chroot() {

    # Lay down the minimal system files so we can bootstrap ourselves
    # Find and extract the minimal system files - one-by-one
    # from: OSC_IMAGE_DIR and OSC_MINSYS set in opensuse-ver-extra dir
    ( cd $CHROOT; 
    for arcfile in $OSC_MINSYS; do
	if [ -e "$IMAGE_DIR/$OSC_IMAGE_DIR/$arcfile"  ]; then
           xz -d < "$IMAGE_DIR/$OSC_IMAGE_DIR/$arcfile" | sudo tar xf -
        else
           die "Did not find essential $IMAGE_DIR/$arcfile file."
        fi
    done )
    # fix up the chroot to make sure we can use it
    sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"
    for d in /proc /sys /dev /dev/pts /dev/shm; do
        [[ -L $d ]] && d="$(readlink -f "$d")"
        sudo mkdir -p "${CHROOT}$d"
        sudo mount --bind "$d" "${CHROOT}$d"
    done

    [[ $USE_PROXY = "1" ]] && (
        cd "$CHROOT"
        for f in etc/zypp/repos.d/*; do
            [[ -f "$f" ]] || continue
            in_chroot /bin/grep -q "'^proxy='" "/$f" && continue
            in_chroot /bin/grep -q "'^baseurl=http://.*127\.0\.0\.1.*'" "/$f" && \
                continue
            in_chroot sed -i "'/^name/ a\proxy=http://$PROXY_HOST:$PROXY_PORT'" "$f"
            [[ $PROXY_USER ]] && \
                in_chroot sed -i "'/^proxy/ a\proxy_username=$PROXY_USER'" "$f"
            [[ $PROXY_PASSWORD ]] && \
                in_chroot sed -i "'^/proxy_username/ a\proxy_password=$PROXY_PASSWORD'" "$f"
            : ;
        done
    )
    # Make sure that the bootstrap only uses local data off the install DVD.
    in_chroot mkdir -p /etc/zypp/repos.d/
    in_chroot mkdir -p /packages/base
    sudo mount --bind "$IMAGE_DIR" "$CHROOT/packages/base"
    make_repo_file opensuse-base 99 "file:///packages/base/$OS_REPO_POOL"
    # have zypper bootstrap everything else into usefulness
    in_chroot zypper install -y -l -f zypper createrepo
    # If we are using a proxy, fastestmirror usually does the Wrong Thing.
    [[ $USE_PROXY = 1 && \
        -f $CHROOT/etc/sysconfig/proxy ]] && \
        in_chroot sed -ie "'/^PROXY_ENABLED/ s/yes/no/'" /etc/sysconfig/proxy
#    if [[ $ALLOW_CACHE_UPDATE = true ]]; then
#        in_chroot 'cp /etc/zypp/repos.d.old/* /etc/zypp/repos.d'
#    fi
}

# Extract version information from an RPM file
rpmver() {
    [[ -f $1 && $1 = *.rpm ]] || die "$1 is not an rpm!"
    if [[ ! ${SEEN_RPMS["${1##*/}"]} ]]; then
        local ver=$(rpm --queryformat \
            '%{NAME}.%{ARCH} %{EPOCH}:%{VERSION}-%{RELEASE}' \
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

# Copy our isolinux bits into place and add our autoinst files to the BUILD_DIR root
# also, update our isolinux config files to provide the autoyast entries needed
final_build_fixups() {
    # Copy our autoyast installation control files.
    debug "Updating the autoyast installation control boot information"
    debug "Adding our autoyast control files to the $BUILD_DIR root."
    cp -a "$BUILD_DIR/extra/autoinst.xml" $BUILD_DIR/
    cp -a "$BUILD_DIR/extra/pxe-inst.xml" $BUILD_DIR/
}

# Build our ISO image.
build_iso() (
    cd "$BUILD_DIR"
    find -name '.svn' -type d -exec rm -rf '{}' ';' 2>/dev/null >/dev/null
    find . -type f -not -name isolinux.bin -not -name sha1sums \
        -not -name crowbar.json -not -path '*/.git/*' -print0 | \
        xargs -0 -- sha1sum -b >sha1sums
    [[ ! -d $ISO_DEST ]] && mkdir -p "$ISO_DEST"
    # Save the sha1sums and the build-info files along side the iso.
    cp sha1sums build-info "$ISO_DEST"
    if ! [[ $NO_GENERATE_ISO && $NO_GENERATE_ISO = true ]]; then
        sudo mkisofs -r -V "${VERSION:0:30}" -cache-inodes -J -l -quiet \
            -b boot/x86_64/loader/isolinux.bin -c boot/x86_64/loader/boot.cat -joliet-long \
            -no-emul-boot --boot-load-size 4 -boot-info-table \
            -o "$ISO_DEST/$BUILT_ISO" "$IMAGE_DIR" "$BUILD_DIR"
    fi
)

# Preparation of the local copy of the image boot handler
# and exclusion of the original copy in the IMAGE_DIR
# so it will not be merged into the final ISO
iso_boot_prep() {
    # OpenSUSE puts isolinux code under ~/boot
    # We need to accommodate this layout
    [[ -e "$IMAGE_DIR/boot" ]] || die "Boot loader on DVD source image is not where it is expected!"
    mkdir -p "$BUILD_DIR/boot"
    chmod u+wr "$BUILD_DIR/boot"
    rsync -rl --ignore-existing --inplace \
        "$IMAGE_DIR/boot" "$BUILD_DIR"
    chmod -R u+wr "$BUILD_DIR/boot"
# The following may be needed if ever it is necessary to inject the autoyast XML file into the initrd
# - this was deemed not necessary at this time, thus commented out.
#    cp -a "$IMAGE_DIR/boot/x86_64/loader/initrd" "$BUILD_DIR/initrd.cpio.gz"
#    gunzip "$BUILD_DIR/initrd.cpio.gz"
#    mkdir "$BUILD_DIR/tmp"
#    (cd "$BUILD_DIR/tmp"; sudo bash -c 'cat ../initrd.cpio | cpio -idm'; \
#         cp "$BUILD_DIR/extra/autoinst.xml" "$BUILD_DIR/tmp/")
#    (cd "$BUILD_DIR/tmp"; sudo bash -c 'find . '!' -name '*~' | cpio --quiet --create -H newc > ../initrd-new.cpio')
#    (cd "$BUILD_DIR"; gzip initrd-new.cpio)
#    (cd $BUILD_DIR; rm "$BUILD_DIR/boot/x86_64/loader/initrd"; cp initrd-new.cpio.gz  "$BUILD_DIR/boot/x86_64/loader/initrd")
    [[ ! -e "$BUILD_DIR/boot/x86_64/loader/" ]] && die "Boot loader in BUILD_DIR=$BUILD_DIR is not where we expect it to be!"
    cp -a "$BUILD_DIR/extra/isolinux.cfg" "$BUILD_DIR/boot/x86_64/loader/"
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/boot"
}

get_boot_info() {
    if [[ -e ${LOOPDIR}/isolinux ]]; then
        TARGET=${LOOPDIR}/isolinux
    elif
    [[ -e ${LOOPDIR}/boot/x86_64/loader ]]; then
        TARGET=${LOOPDIR}/boot/x86_64/loader
    else
        die "DVD isolinux not found on $TARGET"
    fi

    while read line; do
        [[ ! $kernel && ( $line =~ $kernel_re ) ]] && \
            kernel="${BASH_REMATCH[1]}" || :
        [[ ! $kernel_params && ( $line =~ $append_re ) ]] && \
            kernel_params=${BASH_REMATCH[2]} || :
        [[ ! $initrd && $kernel_params && ( $kernel_params =~ $initrd_re ) ]] && {
            kernel_params=${kernel_params/append=${BASH_REMATCH[1]}/}
            initrd="${BASH_REMATCH[1]}"
        } || :
    done < "$TARGET/isolinux.cfg"

    # Fix up our paths to the initrd and the kernel
    for d in "$LOOPDIR/boot/x86_64/loader" "$LOOPDIR"; do
        [[ -f $d/$kernel && -f $d/$initrd ]] || continue
        kernel="$d/$kernel"
        initrd="$d/$initrd"
        break
    done
}


__check_all_deps() {
    local pkgname pkg token rest bc ok line
    local -a lines
    local token_re='(package|provider):[[:space:]]*([^[:space:]]+)'
    for pkgname in "$@"; do
        local -A deps
        [[ ${touched_pkgs["$pkgname"]} ]] && continue
        #debug "Checking dependencies for $pkgname"
        mapfile -t -n 0 lines < <(in_chroot zypper what-provides "$pkgname")
        for line in "${lines[@]}"; do
            [[ $line =~ $token_re ]] || continue
            token=${BASH_REMATCH[1]}
            pkg=${BASH_REMATCH[2]}
            ok=false
            # We only care about certian arches.
            for rest in "${PKG_ALLOWED_ARCHES[@]}"; do
                [[ $pkg = *.$rest ]] || continue
                ok=true
                break
            done
            [[ $ok = true ]] || continue
            case $token in
                package)
                    [[ ${CD_POOL["${pkg}"]} && \
                        ! ${INSTALLED_PKGS["${pkg}"]} ]] || continue
                    #debug "Staging depended upon package $pkg"
                    INSTALLED_PKGS["${pkg}"]="true"
                    touched_pkgs["${pkg}"]="true";;
                provider) [[ ${seen_deps["$pkg"]} ]] && continue
                    #debug "Will check dependent package ${pkg} for $pkgname"
                    INSTALLED_PKGS["${pkg}"]="true"
                    seen_deps["$pkg"]="true"
                    deps["${pkg}"]="true";;
                *) continue;;
            esac
        done
        [[ ${!deps[*]} ]] && __check_all_deps "${!deps[@]}"
        unset deps
        # Add ourselves to the installed_pkgs array if we would not
        # have been processed by yum above.
        for rest in "${PKG_ALLOWED_ARCHES[@]}"; do
            [[ $pkgname = *.$rest ]] || continue
            INSTALLED_PKGS["$pkgname"]="true"
            break
        done
        printf '.' >&2
    done
}

check_all_deps() {
    local -A touched_pkgs
    local -A seen_deps
    debug "Analyzing dependencies for shrinking the install ISO."
    __check_all_deps "$@"
}

# Throw away packages we will not need on the install media
shrink_iso() {
    # Do nothing if we do not have a minimal-install set for this OS.
    [[ -f $CROWBAR_DIR/$OS_TOKEN-extra/minimal-install ]] || \
        return 0
    local pkgname pkgver compfile
    local -a minimal to_copy
    while read pkgname pkgver; do
        minimal+=("$pkgname")
    done < "$CROWBAR_DIR/$OS_TOKEN-extra/minimal-install"
    mkdir -p "$BUILD_DIR/Packages"
    cp -a "$IMAGE_DIR/repodata" "$BUILD_DIR"
    make_chroot
    # Make sure all our dependencies will be staged.
    check_all_deps $(printf "%s\n" "${minimal[@]}" \
        $(for bc in "${BARCLAMPS[@]}"; do echo ${BC_PKGS[$bc]}; done) | \
        sort -u)
    for pkgname in "${!INSTALLED_PKGS[@]}"; do
        [[ -f ${CD_POOL["$pkgname"]} ]] || continue
        to_copy+=("${CD_POOL["$pkgname"]}")
    done
    debug "Final ISO default CD pool will be shrunk down to:"
    printf "  %s\n" "${to_copy[@]##*/}" |sort >&2
    if [[ $OS = fedora ]]; then
        for pkgname in "${to_copy[@]}"; do
            # Reorganize packages for fedora ver > 17
            lowercase_pkgname="${pkgname,,}"
            mkdir -p "$BUILD_DIR/Packages/${lowercase_pkgname:0:1}"
            cp "${CD_POOL["$pkgname"]}" "$BUILD_DIR/Packages/${lowercase_pkgname:0:1}"
        done
    else
        cp "${to_copy[@]}" "$BUILD_DIR/Packages"
    fi
    sudo mount --bind "$BUILD_DIR" "$CHROOT/mnt"
    compfile=$(in_chroot find /mnt/repodata -name '*-comps*.xml')
    in_chroot "cd /mnt; createrepo -g  \"$compfile\" ."
    sudo umount -l "$CHROOT/mnt"
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/Packages"
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/repodata"
}

generate_minimal_install() { : ; }

# Check to make sure our required commands are installed.
for cmd in rpm; do
    which "$cmd" &>/dev/null || \
        die 1 "Please install $cmd before trying to build Crowbar."
done
