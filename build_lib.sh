#!/bin/bash

# Library of useful functions for building Crowbar.
# This is sourced by build_crowbar.sh and any other interested parties.

# If we have already been sourced in the current env, don't source us again.
[[ $CROWBAR_BUILD_SOURCED = true ]] && exit 0

[[ $DEBUG ]] && {
    set -x
    export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
}

# We might use lots and lots of open files.  Bump our open FD limits.
ulimit -Sn unlimited

# Hashes to hold our "interesting" information.
# Key = barclamp name
# Value = whatever interesting thing we are looking for.
declare -A BC_DEPS BC_GROUPS BC_PKGS BC_EXTRA_FILES BC_OS_DEPS BC_GEMS
declare -A BC_REPOS BC_PPAS BC_RAW_PKGS BC_BUILD_PKGS BC_QUERY_STRINGS
declare -A BC_SMOKETEST_DEPS BC_SMOKETEST_TIMEOUTS BC_BUILD_CMDS
declare -A BC_SUPERCEDES BC_SRC_PKGS

# Build OS independent query strings.
BC_QUERY_STRINGS["deps"]="barclamp requires"
BC_QUERY_STRINGS["groups"]="barclamp member"
BC_QUERY_STRINGS["extra_files"]="extra_files"
BC_QUERY_STRINGS["build_cmd"]="build_cmd"
BC_QUERY_STRINGS["os_support"]="barclamp os_support"
BC_QUERY_STRINGS["gems"]="gems pkgs"
BC_QUERY_STRINGS["test_deps"]="smoketest requires"
BC_QUERY_STRINGS["test_timeouts"]="smoketest timeout"
BC_QUERY_STRINGS["supercedes"]="barclamp supercedes"

# By default, do not try to update the cache or the metadata.
# These will be unset if --update-cache is passed to the build.
ALLOW_CACHE_UPDATE=false
ALLOW_CACHE_METADATA_UPDATE=false


get_barclamp_info() {
    local bc yml_file line query newdeps dep d i
    local new_barclamps=()
    # Pull in interesting information from all our barclamps
    cd "$CROWBAR_DIR"
    for bc in barclamps/*; do
        [[ -d "$bc" ]] || continue
        bc=${bc##*/}
        debug "Reading metadata for $bc barclamp."
        is_barclamp "$bc" || {
            echo "$bc is not a barclamp, skipping."
            continue
        }
        yml_file="$CROWBAR_DIR/barclamps/$bc/crowbar.yml"
        [[ $bc = crowbar ]] || BC_DEPS["$bc"]+="crowbar "
        for query in "${!BC_QUERY_STRINGS[@]}"; do
            while read line; do
                [[ $line = nil ]] && continue
                case $query in
                    deps) is_in "$line "${BC_DEPS["$bc"]} || \
                        BC_DEPS["$bc"]+="$line ";;
                    groups) is_in "$line" ${BC_GROUPS["$bc"]} ||
                        BC_GROUPS["$line"]+="$bc ";;
                    pkgs|os_pkgs) is_in "$line" ${BC_PKGS["$bc"]} || \
                        BC_PKGS["$bc"]+="$line ";;
                    src_pkgs) is_in "$line" ${BC_SRC_PKGS["$bc"]} || \
                        BC_SRC_PKGS["$bc"]+="$line ";;
                    extra_files) BC_EXTRA_FILES["$bc"]+="$line\n";;
                    os_support) BC_OS_SUPPORT["$bc"]+="$line ";;
                    gems) BC_GEMS["$bc"]+="$line ";;
                    repos|os_repos) BC_REPOS["$bc"]+="$line\n";;
                    ppas|os_ppas) [[ $PKG_TYPE = debs ]] || \
                        die "Cannot declare a PPA for $PKG_TYPE!"
                        BC_REPOS["$bc"]+="ppa $line\n";;
                    build_pkgs|os_build_pkgs) BC_BUILD_PKGS["$bc"]+="$line ";;
                    raw_pkgs|os_raw_pkgs|pkg_sources|os_pkg_sources) BC_RAW_PKGS["$bc"]+="$line ";;
                    test_deps) BC_SMOKETEST_DEPS["$bc"]+="$line ";;
                    os_build_cmd|build_cmd) [[ ${BC_BUILD_CMDS["$bc"]} ]] && \
                        die "Only one os_build_cmd stanza per OS per barclamp allowed!"
                        BC_BUILD_CMDS["$bc"]="$line";;
                    test_timeouts) BC_SMOKETEST_TIMEOUTS["$bc"]+="$line ";;
                    supercedes) 
                        [[ ${BC_SUPERCEDES[$line]} ]] && \
                            die "$line is already superceded by ${BC_SUPERCEDES[$line]}!"
                        BC_SUPERCEDES["$line"]="$bc";;
                    *) die "Cannot handle query for $query."
                esac
            done < <("$CROWBAR_DIR/parse_yml.rb" \
                "$yml_file" \
                ${BC_QUERY_STRINGS["$query"]} 2>/dev/null)
        done
    done
    cd -

    debug "Analyzing barclamp group membership"
    # If any barclamps need group expansion, do it.
    for bc in "${!BC_DEPS[@]}"; do
        newdeps=''
        for dep in ${BC_DEPS["$bc"]}; do
            if [[ $dep = @* ]]; then
                [[ ${BC_GROUPS["${dep#@}"]} ]] || \
                    die "$bc depends on group ${dep#@}, but that group does not exist!"
                for d in ${BC_GROUPS["${dep#@}"]}; do
                    newdeps+="$d "
                done
            else
                newdeps+="$dep "
            fi
        done
        BC_DEPS["$bc"]="$newdeps"
    done

    # Group-expand barclamps if needed, and unset groups after they are expanded
    for i in "${!BARCLAMPS[@]}"; do
        bc="${BARCLAMPS[$i]}"
        if [[ $bc = @* ]]; then
            [[ ${BC_GROUPS["${bc#@}"]} ]] || \
                die "No such group ${bc#@}!"
            BARCLAMPS+=(${BC_GROUPS["${bc#@}"]})
            unset BARCLAMPS[$i]
        else
            is_barclamp "$bc" || die "$bc is not a barclamp!"
        fi
    done
    BARCLAMPS=("${BARCLAMPS[@]//@*}")

    # Pull in dependencies for the barclamps.
    # Everything depends on the crowbar barclamp, so include it first.
    new_barclamps=("crowbar")
    while [[ t = t ]]; do
        for bc in "${BARCLAMPS[@]}"; do
            if [[ ${BC_SUPERCEDES[$bc]} ]]; then
                debug "$bc is superceded by ${BC_SUPERCEDES[$bc]}. Skipping."
                continue
            fi
            for dep in ${BC_DEPS["$bc"]}; do
                dep="${BC_SUPERCEDES[$dep]:-$dep}"
                is_barclamp "$bc" || die "$bc depends on $dep, which is not a barclamp!"
                is_in "$dep" "${new_barclamps[@]}" && continue
                new_barclamps+=("$dep")
            done
            is_barclamp "$bc" || die "$bc is not a barclamp!"
            is_in "$bc" "${new_barclamps[@]}" || new_barclamps+=("$bc")
        done
        [[ ${BARCLAMPS[*]} = ${new_barclamps[*]} ]] && break
        BARCLAMPS=("${new_barclamps[@]}")
    done
}

[[ $CROWBAR_BUILD_PID ]] || export CROWBAR_BUILD_PID=$$
export CLEANUP_LOCK="$CROWBAR_DIR/.cleanup.lock"
cleanup_cmds=()

git_managed_cache() [[ -d $CACHE_DIR/.git ]]

with_build_lock() {
    flock -n 65 || die "Could not grab build lock!"
    "$@"
} 65>/tmp/.build_crowbar.lock

# Get a list of all the barclamps that a specific branch refers to.
barclamps_in_branch() {
    local b res=()
    for b in "$@"; do
        in_repo branch_exists "$b" || \
            die "Branch $b does not exist in the Crowbar repo!"
    done
    local res=($(for b in "$@"; do in_repo git ls-tree -r \
        "$b" barclamps; done | \
        awk '/160000 commit/ {print $4}' |sort -u))
    printf "%s\n" "${res[@]#barclamps/}"
}


# Our general cleanup function.  It is called as a trap whenever the
# build script exits, and it's job is to make sure we leave the local
# system in the same state we cound it, modulo a few calories of wasted heat
# and a shiny new .iso.
cleanup() {
    flock -n 70 || exit 1
    # Clean up any stray mounts we may have left behind.
    # The paranoia with the grepping is to ensure that we do not
    # inadvertently umount everything.
    if [[ $BASH_SUBSHELL -gt 0 || $BASHPID != $CROWBAR_BUILD_PID ]]; then
        kill -INT $CROWBAR_BUILD_PID
        exit 1
    fi
    for c in "${cleanup_cmds[@]}"; do
        $c || res=1
    done

    GREPOPTS=()
    [[ $CACHE_DIR ]] && GREPOPTS=(-e "$CACHE_DIR")
    [[ $IMAGE_DIR && $CACHE_DIR =~ $IMAGE_DIR ]] && GREPOPTS+=(-e "$IMAGE_DIR")
    [[ $BUILD_DIR && $CACHE_DIR =~ $BUILD_DIR ]] && GREPOPTS+=(-e "$BUILD_DIR")
    [[ $CROWBAR_DIR && -d $CROWBAR_DIR/testing ]] && GREPOPTS+=(-e "$CROWBAR_DIR/testing")
    [[ $CHROOT && $CACHE_DIR =~ $CHROOT ]] && GREPOPTS+=(-e "$CHROOT")
    if [[ $GREPOPTS ]]; then
        while read dev fs type opts rest; do
            sudo umount -d -l "$fs"
        done < <(tac /proc/self/mounts |grep "${GREPOPTS[@]}")
    fi
    # If the build process spawned a copy of webrick, make sure it is dead.
    [[ $webrick_pid && -d /proc/$webrick_pid ]] && kill -9 $webrick_pid
    # clean up after outselves from merging branches, if needed.
    [[ $CI_BARCLAMP ]] &&  {
        if ! in_repo git submodule update -N "barclamps/$CI_BARCLAMP"; then
            in_ci_barclamp git checkout -f master
        fi
        if ! in_ci_barclamp git branch -D ci-throwaway-branch; then
            in_ci_barclamp git checkout -f master
            in_ci_barclamp git branch -D ci-throwaway-branch
        fi

    }
    cd "$CROWBAR_DIR"
    if [[ $THROWAWAY_BRANCH ]]; then
        # Check out the branch we started the build process, and then
        # nuke whatever throwaway branch we may have created.
        git checkout -f "${CURRENT_BRANCH#refs/heads/}" &>/dev/null
        git branch -D "$THROWAWAY_BRANCH" &>/dev/null
    fi
    # If we saved unadded changes, resurrect them.
    [[ $THROWAWAY_STASH ]] && git stash apply "$THROWAWAY_STASH" &>/dev/null
    # Do the same thing as above, but for the build cache instead.
    mkdir -p "$CACHE_DIR"
    cd "$CACHE_DIR"
    if ! in_cache git diff-index --cached --quiet HEAD; then
        in_cache git commit -m "Updated by build_crowbar.sh @ $(date) for ${OS_TOKEN}"
        echo "The crowbar build cache has been updated, and the updates have"
        echo "been comitted back to the cache.  Please push any changes."
    fi
    wait
    flock -u 70
    rm "$CROWBAR_DIR/".*.lock
    exit $res
} 70> "$CLEANUP_LOCK"

# Arrange for cleanup to be called at the most common exit points.
trap cleanup 0 INT QUIT TERM

# Test to see if $1 is in the rest of the args.
is_in() {
    local t="(^| )$1( |\$)"
    shift
    [[ $* =~ $t ]]
}

# Run a command in our chroot environment.
in_chroot() { sudo -H /usr/sbin/chroot "$CHROOT" "$@"; }

# A little helper function for doing bind mounts.
bind_mount() {
    [[ -d $2 ]] || mkdir -p "$2"
    grep -q "$2" /proc/self/mounts || sudo mount --bind "$1" "$2"
}

# Write a string with \n translated to a real line break.
write_lines() { [[ $1 ]] && printf "%b" "$1"; }

# Read base repository information from the *.list files.
read_base_repos() {
    for pkgfile in "$BUILD_DIR/extra/packages/"*.list; do
        [[ -f $pkgfile ]] || continue
        while read line; do
            line=${line%%#*}
            [[ $line ]] || continue
            if [[ $line = repository* ]]; then
                REPOS+=("${line#* }")
            else
                echo "$line in $pkgfile has been superceded by crowbar.yml metadata." >&2
                die "Please migrate $line into the appropriate barclamp config file."
            fi
        done <"$pkgfile"
    done
}

# Worker function for all_deps
__all_deps() {
    local dep
    if [[ ${BC_DEPS["$1"]} ]]; then
        for dep in ${BC_DEPS["$1"]}; do
            is_in "$dep" "${deps[@]}" && continue
            __all_deps "$dep"
        done
    fi
    is_in "$1" "${deps[@]}" || deps+=("$1")
    return 0
}

# Given a barclamp, echo all of the dependencies of that barclamp
all_deps() {
    local deps=() dep
    __all_deps "$1"
    echo "${deps[*]}"
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

# Compare a version string, handling mixed numeric and alpha sequences
# as appropriate.
vercmp(){
    # $1 = version string of first package
    # $2 = version string of second package
    # Returns 0 if $1 > $2, 1 otherwise.
    # urldecode the version strings.
    local a="$(printf "%b" "${1//%/\\x}")"
    local b="$(printf "%b" "${2//%/\\x}")"
    # prepend a version epoch if one is not already there.
    [[ $a =~ ^[0-9]+: ]] || a="0:$a"
    [[ $b =~ ^[0-9]+: ]] || b="0:$b"
    local ver1=()
    local ver2=()
    local i=0
    # split the version strings into arrays using the usual field splitters
    IFS=':.-_ +' read -rs -a ver1 <<< "$a"
    IFS=':.-_ +' read -rs -a ver2 <<< "$b"
    for ((i=0;;i++)); do
        __cmp "${ver1[$i]}" "${ver2[$i]}"
        case $? in
            2) return 0;;
            0) return 1;;
            255) return 1;;
        esac
    done

}

# Index the pool of packages in the CD.
index_cd_pool() {
    # Scan through our pool to find pkgs we can easily omit.
    local pkgname='' pkg='' cache="$CACHE_DIR/$OS_TOKEN/iso-packages"
    if [[ $ISO_LIBRARY/$ISO -nt $cache ]]; then
        mkdir -p "${cache%/*}"
        > "$cache"
        while read pkg; do
            [[ -f $pkg ]] && is_pkg "$pkg" || continue
            pkgname="$(pkg_name "$pkg")"
            CD_POOL["$pkgname"]="${pkg}"
            echo "CD_POOL[\"$pkgname\"]=\"${pkg}\"" >> "$cache"
        done < <(find "$(find_cd_pool)" -type f)
    else
        . "$cache"
    fi
}

# Make a chroot environment for package-fetching purposes.
make_chroot() {
    [[ -f $CHROOT/etc/resolv.conf ]] && return 0
    local bc repo
    debug "Making utility chroot"
    sudo mkdir -p "$CHROOT/$CHROOT_PKGDIR"
    sudo mkdir -p "$CHROOT/$CHROOT_GEMDIR"
    __make_chroot
    in_chroot ln -s /proc/self/mounts /etc/mtab

    if [[ $ALLOW_CACHE_UPDATE = true ]]; then
        read_base_repos
        # Add our basic repositories
        add_repos "${REPOS[@]}" || \
            die "Could not add base repositories for $OS_TOKEN chroot!"

        # Add the repos from the barclamps.
        # We do it here because importing the metadata takes forever
        # if we refresh on every barclamp.
        for bc in "${BARCLAMPS[@]}"; do
            while read repo; do
                add_repos "$repo"
            done < <(write_lines "${BC_REPOS[$bc]}")
        done
    fi
    add_offline_repos
    chroot_update
}

stage_pkgs() {
    # $1 = cache to copy from.
    # $2 = location to copy to
    local pkg pkgname pkg_t
    local -A to_copy STAGED_POOL
    while read pkg; do
        # If it is not a package, skip it.
        is_pkg "$pkg" || continue
        pkgname="$(pkg_name "$pkg")"
        # Check to see if it is in the CD pool.
        pkg_t="${CD_POOL["$pkgname"]}"
        # If it is, and the one in the pool is not older than this one,
        # skip it.
        if [[ $pkg_t && -f $pkg_t ]] && ( ! pkg_cmp "$pkg" "$pkg_t" ); then
            #debug "Skipping copy of ${pkg##*/}, it is on the install media"
            # if we are shrinking our ISO, make sure this one is in.
            [[ $SHRINK_ISO = true ]] && INSTALLED_PKGS["$pkgname"]="true"
            continue
        fi
        # Now check to see if we have already staged it
        pkg_t="${STAGED_POOL["$pkgname"]}"
        if [[ $pkg_t && -f $pkg_t ]]; then
            # We have already staged it.  Check to see if ours is newer than
            # the one already staged.
            if pkg_cmp "$pkg" "$pkg_t"; then
                # We are newer.  Delete the old one, copy us,
                # and update $STAGED_POOL
                #debug "Replacing ${pkg_t##*/} with ${pkg##*/}"
                [[ -f "$pkg_t" ]] && rm -f "$pkg_t"
                [[ ${to_copy["$pkg_t"]} ]] && unset to_copy["$pkg_t"]
                to_copy["$pkg"]="true"
                STAGED_POOL["$pkgname"]="$2/${pkg##*/}"
            fi
        else
            # We have not seen this package before.  Copy it.
            to_copy["$pkg"]="true"
            cp "$pkg" "$2"
            STAGED_POOL["$pkgname"]="$2/${pkg##*/}"
        fi
    done < <(find "$1" -type f)
    [[ ${!to_copy[*]} ]] && cp "${!to_copy[@]}" "$2"
}

cache_add() {
    # $1 = file to add.
    # $2 = location to store it in the cache
    cp "$1" "$2" || \
        die "Cannot save $1 in $2!"
    debug "cache_add: Saved $1 in $2"
    if [[ $CURRENT_CACHE_BRANCH ]]; then
        CACHE_NEEDS_COMMIT=true
        in_cache git add "${2#${CACHE_DIR}/}"
    	debug "in_cache git add result: $?"
    fi
}

cache_rm() {
    if [[ $CURRENT_CACHE_BRANCH ]]; then
        CACHE_NEEDS_COMMIT=true
        in_cache git rm -f "${1#${CACHE_DIR}/}"
    fi
    rm -f "$1"
}

make_barclamp_pkg_metadata() {
    [[ $ALLOW_CACHE_UPDATE != true && \
        $ALLOW_CACHE_METADATA_UPDATE != true ]] && return 0
    [[ -d $CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs ]] || return 0
    if [[ $force_update != true ]]; then
        __barclamp_pkg_metadata_needs_update "$1" || return 0
    fi
    [[ $ALLOW_CACHE_METADATA_UPDATE = false ]] && \
        die "Need to update cache metadata for $1, but --no-metadata-update passed."
    debug "Updating package cache metadata for $1"
    make_chroot
    [[ $OS_METADATA_PKGS ]] && {
        chroot_install $OS_METADATA_PKGS
        unset OS_METADATA_PKGS
    }
    sudo mount --bind "$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs" "$CHROOT/mnt"
    __make_barclamp_pkg_metadata "$1"
    sudo umount "$CHROOT/mnt"
}

install_build_packages() {
    chroot_install ${BC_BUILD_PKGS["$1"]}
}

update_barclamp_src_pkg_cache() {
    [[ ${BC_SRC_PKGS["$1"]} ]] || return 0
    local CHROOT_PKGDIR="tmp/$1"
    in_chroot mkdir -p "/$CHROOT_PKGDIR"
    chroot_fetch_source ${BC_SRC_PKGS["$1"]}
    local p
    for p in "$CHROOT/$CHROOT_PKGDIR/"*; do
        [[ -f $p ]] || continue
        cache_add "$p" "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/${p##*/}"
    done
}

# Update the package cache for a barclamp.
update_barclamp_pkg_cache() {
    # $1 = barclamp we are working with
    local bc_cache="$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs" pkg dest bc
    local -A pkgs
    # Wipe out the packages already in the chroot package directory.
    while read pkg; do
        is_pkg "$pkg" && sudo rm -f "$pkg"
    done < <(find "$CHROOT/$CHROOT_PKGDIR" -type f)
    for bc in $(all_deps "$1"); do
        [[ -d "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/." ]] || continue
        sudo cp -a "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/." \
            "$CHROOT/$CHROOT_PKGDIR"
    done
    # Remember what packages we already have.
    while read pkg; do
        is_pkg "$pkg" || continue
        pkgs["$pkg"]="true"
    done < <(cd "$CHROOT/$CHROOT_PKGDIR"; find -type f)
    [[ ${BC_BUILD_PKGS["$1"]} ]] && install_build_packages "$1"
    chroot_fetch ${BC_PKGS["$1"]} || \
        die "Could not fetch packages required by barclamp $1"
    mkdir -p "$bc_cache"
    while read pkg; do
        is_pkg "$CHROOT/$CHROOT_PKGDIR/$pkg" || continue
        [[ ${pkgs["$pkg"]} = true ]] && continue
        if [[ ${pkg%/*} != '.' ]]; then
            [[ -d $bc_cache/${pkg%/*} ]] || mkdir -p "$bc_cache/${pkg%/*}"
            [[ -f $bc_cache/${pkg##*/} ]] && cache_rm "$bc_cache/${pkg##*/}"
        fi
        cache_add "$CHROOT/$CHROOT_PKGDIR/$pkg" "$bc_cache/${pkg//%3a/:}"
    done < <(cd "$CHROOT/$CHROOT_PKGDIR"; find -type f)
    local force_update=true
    update_barclamp_src_pkg_cache "$1"
    make_barclamp_pkg_metadata "$1"
}

# Update the gem cache for a barclamp
update_barclamp_gem_cache() {
    local -A gems
    local gemname gemver gemopts bc gem
    local bc_cache="$CACHE_DIR/barclamps/$1/gems"
    CHROOT_GEMDIR="$(in_chroot /usr/bin/gem environment gemdir | grep -v home)"

    debug "update_barclamp_gem_cache: starting to ${1} to $bc_cache"

    # Wipe out the caches.
    debug "destroy that cruft"
    ( cd "$CHROOT/$CHROOT_GEMDIR" && sudo rm -rf * )
    debug "$(ls -al ${CHROOT}/${CHROOT_GEMDIR})"

    # if we have deb or gem caches, copy them back in.
    # Make sure we copy the caches for all our dependent barclamps.
    
    in_chroot mkdir -p "${CHROOT}/${CHROOT_GEMDIR}" || die "could not make ${CHROOT}/${CHROOT_GEMDIR}"

    debug "copy from $CACHE_DIR/barclamps/$bc/gems/. to  $CHROOT/$CHROOT_GEMDIR"
    for bc in $(all_deps "$1"); do
	debug "are there any gems? sudo ls $CACHE_DIR/barclamps/$bc/gems/."
	debug "$(sudo ls ${CACHE_DIR}/barclamps/${bc}/gems/.)"
        [[ -d "$CACHE_DIR/barclamps/$bc/gems/." ]] && \
            sudo cp -va "$CACHE_DIR/barclamps/$bc/gems/." \
            "$CHROOT/$CHROOT_GEMDIR" 
	debug "did it make it into chroot?"
	debug "$(in_chroot ls -al /${CHROOT_GEMDIR})"
    done

    debug "create a hash of gems"
    while read gem; do
	debug "gem ${gem} is from ${CHROOT}/${CHROOT_GEMDIR}"
	debug "gem ${gem} sure does end in .gem"
        [[ $gem = *.gem ]] || continue
        gems["$gem"]="true"
    done < <(sudo find "$CHROOT/$CHROOT_GEMDIR" -type f)

    # install any build dependencies we need.
    debug "install build packages $1"
    install_build_packages "$1"

    # Grab the gems needed for this barclamp.
    debug "installing gems needed in this barclamp $1"
    for gem in ${BC_GEMS["$1"]}; do
	debug "gem ${gem} is required by barclamp $1"
        if [[ $gem =~ $GEM_RE ]]; then
            gemname="${BASH_REMATCH[1]}"
            gemver="${BASH_REMATCH[2]}"
        else
            gemname="$gem"
            gemver=''
        fi
        gemopts=(install --no-ri --no-rdoc)
        [[ $gemver ]] && gemopts+=(--version "= ${gemver}")
        [[ $http_proxy ]] && gemopts+=(-p "$http_proxy")
        in_chroot /usr/bin/gem "${gemopts[@]}" "$gemname"
    done
    debug "finished installing the gems"

    # Save our updated gems and pkgs in the cache for later.
    mkdir -p "$bc_cache" || die "Failed to make gem cache ${bc_cache}"
    debug "made the gem cache ${bc_cache}"


    debug "adding gems to cache"
    debug "$(sudo ls -la $CHROOT/$CHROOT_GEMDIR)"
    while read gem; do
    	debug "gem ${gem} is from $CHROOT/$CHROOT_GEMDIR" 
        [[ $gem = *.gem ]] || continue
	debug "gem ${gem} sure does end in .gem"
        [[ ${gems["$gem"]} = "true" ]] && continue
	debug "gem hash ${gem} = ${gems[${gem}]} "
	debug "update_barclamp_gem_cache: adding ${gem} to $bc_cache"
        cache_add "$gem" "$bc_cache"
    done < <(sudo find "$CHROOT/$CHROOT_GEMDIR" -type f)
    debug "done adding gems to cache"
}

# Fetch any raw packages we do not already have.
update_barclamp_raw_pkg_cache() {
    local pkg bc_cache="$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs"
    mkdir -p "$bc_cache"
    # Fetch any raw_pkgs we were asked to.
    for pkg in ${BC_RAW_PKGS["$1"]} ${BC_PKG_SOURCES["$1"]}; do
        [[ -f $bc_cache/${pkg##*/} ]] && continue
        echo "Caching $pkg:"
        curl -L -o "$bc_cache/${pkg##*/}" "$pkg"
        [[ $CURRENT_CACHE_BRANCH ]] && in_cache git add "$bc_cache/${pkg##*/}"
    done
    touch "$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs"
}

# Fetch any bare files that we do not already have.
update_barclamp_file_cache() {
    local dest pkg bc_cache="$CACHE_DIR/barclamps/$1/files"
    # Fetch any extra_pkgs we need.
    mkdir -p "$bc_cache"
    while read pkg; do
        dest=${pkg#* }
        [[ $dest = $pkg ]] && dest=''
        pkg=${pkg%% *}
        [[ -f $bc_cache/files/$dest/${pkg##*/} ]] && continue
        mkdir -p "$bc_cache/$dest"
        echo "Caching $pkg:"
        curl -L -o "$bc_cache/$dest/${pkg##*/}" "$pkg"
        [[ $CURRENT_CACHE_BRANCH ]] && \
            in_cache git add "$bc_cache/$dest/${pkg##*/}"
    done < <(write_lines "${BC_EXTRA_FILES[$1]}")
}

# Check to see if the barclamp package cache needs update.
barclamp_pkg_cache_needs_update() {
    local pkg pkgname arch bcs=() bc
    local -A pkgs

    [[ $need_update = true || ${FORCE_BARCLAMP_UPDATE["$1"]} = true ]] && return 0
    [[ -d $CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs ]] && \
        touch "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs"
    # First, check to see if we have all the packages we need.
    for bc in $(all_deps "$1"); do
        [[ -d "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs" ]] && \
            bcs+=("$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs")
    done
    if [[ ${bcs[*]} ]]; then
        while read pkg; do
            is_pkg "$pkg" || continue
            pkgname="$(pkg_name "$pkg")"
        #debug "$pkgname is cached"
            pkgs["$pkgname"]="$pkg"
        done < <(find "${bcs[@]}" -type f)
    fi
    for pkg in ${BC_PKGS["$1"]} ${BC_BUILD_PKGS["$1"]}; do
        [[ $pkg ]] || continue
        for arch in "${PKG_ALLOWED_ARCHES[@]}"; do
            [[ ${pkgs["$pkg-$arch"]} ]] && continue 2
            if [[ ${CD_POOL["$pkg-$arch"]} ]]; then
                INSTALLED_PKGS["$pkg-$arch"]="true"
                continue 2
            fi
        done
        debug "$pkg is not cached, and $1 needs it."
        return 0
    done
    return 1
}

# Check to see if the barclamp gem cache needs an update.
barclamp_gem_cache_needs_update() {
    local pkg pkgname bc
    local -A pkgs
    # Second, check to see if we have all the gems we need.
    for pkg in ${BC_GEMS["$1"]}; do
        for bc in $(all_deps "$1"); do
            local bc_cache="$CACHE_DIR/barclamps/$bc/gems"
            mkdir -p "$bc_cache"
            [[ $(find "$bc_cache" \
                -name "$pkg*.gem" -type f) = *.gem ]] && continue 2
        done
        return 0
    done
    return 1
}

# CHeck to see if we are missing any raw packages.
barclamp_raw_pkg_cache_needs_update() {
    local pkg bc_cache="$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs"
    mkdir -p "$bc_cache"
    # Third, check to see if we have all the raw_pkgs we need.
    for pkg in ${BC_RAW_PKGS["$1"]} ${BC_PKG_SOURCES["$1"]}; do
        [[ -f $bc_cache/${pkg##*/} ]] || return 0
    done
    return 1
}

# Check to see if we are missing any raw files.
barclamp_file_cache_needs_update() {
    local pkg dest bc_cache="$CACHE_DIR/barclamps/$1/files"
    mkdir -p "$bc_cache"
    # Fourth, check to make sure we have all the extra_pkgs we need.
    while read pkg; do
        dest=${pkg#* }
        [[ $dest = $pkg ]] && dest=''
        pkg=${pkg%% *}
        [[ -f $bc_cache/$dest/${pkg##*/} ]] || return 0
    done < <(write_lines "${BC_EXTRA_FILES[$1]}")
    return 1
}

# Some helper functions

# Print a message to stderr and exit.  cleanup will be called.
die() { echo "$(date '+%F %T %z'): $*" >&2; res=1; exit 1; }

# Print a message to stderr and keep going.
debug() { [[ $VERBOSE ]] && echo "$(date '+%F %T %z'): $*" >&2; }

# Clean up any cruft that we might have left behind from the last run.
clean_dirs() {
    local d=''
    for d in "$@"; do
        (   mkdir -p "$d"
            cd "$d"
            sudo rm -rf * )
    done
}

# Verify that the passed name is really a branch in the git repo.
branch_exists() { git show-ref --quiet --verify --heads -- "refs/heads/$1"; }

to_empty_branch() {
    if branch_exists empty-branch; then
        git checkout -q empty-branch
        return $?
    fi
    if [[ -d .git ]]; then
        git symbolic-ref HEAD refs/heads/empty-branch
        rm -f .git/index
    elif [[ -f .git ]]; then
        git checkout --orphan empty-branch
        git rm -r --cached .
    fi
    git clean -f -x -d
    echo "This branch intentionally left blank" >README.empty-branch
    git add README.empty-branch
    git commit -m "Created empty branch"
}

# Run a git command in the crowbar repo.
in_repo() ( cd "$CROWBAR_DIR"; "$@")

# Get the head revision of a git repository.
get_rev() (
    cd "$1"
    if [[ -d .git || -f .git ]]; then
        git rev-parse HEAD
    else
        echo "Not a Git Repository"
    fi
)

# Run a git command in the build cache, assuming it is a git repository.
in_cache() (
    cd "$CACHE_DIR"
    "$@"
)

# Check to see if something is a barclamp.
is_barclamp() { [[ -f "$CROWBAR_DIR/barclamps/$1/crowbar.yml" ]]; }
in_barclamp() {
    (   cd "$CROWBAR_DIR/barclamps/$1"
        shift
        "$@")
}

in_ci_barclamp() {
    [[ $CI_BARCLAMP ]] || die "No continuous integration barclamp!"
    in_barclamp "$CI_BARCLAMP" "$@"
}

# Build our ISO image.
build_iso() (
    cd "$BUILD_DIR"
    rm -f isolinux/boot.cat
    find -name '.svn' -type d -exec rm -rf '{}' ';' 2>/dev/null >/dev/null
    find . -type f -not -name isolinux.bin -not -name sha1sums \
        -not -name crowbar.json -not -path '*/.git/*' -print0 | \
        xargs -0 -- sha1sum -b >sha1sums
    mkdir -p "$ISO_DEST"
        # Save the sha1sums and the build-info files along side the iso.
    cp sha1sums build-info "$ISO_DEST"
    if ! [[ $NO_GENERATE_ISO && $NO_GENERATE_ISO = true ]]; then
        mkisofs -r -V "${VERSION:0:30}" -cache-inodes -J -l -quiet \
            -b isolinux/isolinux.bin -c isolinux/boot.cat -joliet-long \
            -no-emul-boot --boot-load-size 4 -boot-info-table \
            -o "$ISO_DEST/$BUILT_ISO" "$IMAGE_DIR" "$BUILD_DIR"
    fi
)

# Have the smoketest framework do its thing with the ISO we just made.
test_iso() {
    run_test "$@" || \
        die "$(date '+%F %T %z'): Smoketest of $ISO_DEST/$BUILT_ISO failed."
}

export PATH="$PATH:$CROWBAR_DIR:$CROWBAR_DIR/extra:$CROWBAR_DIR/change-image/dell:$CROWBAR_DIR/test_framework"

CROWBAR_BUILD_SOURCED=true
