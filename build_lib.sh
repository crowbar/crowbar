#!/bin/bash

# Library of useful functions for building Crowbar.
# This is sourced by build_crowbar.sh and any other interested parties.

# If we have already been sourced in the current env, don't source us again.
[[ $CROWBAR_BUILD_SOURCED = true ]] && exit 0

[[ $DEBUG ]] && {
    set -x
    export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
}

[[ $CROWBAR_TMP ]] || CROWBAR_TMP=$(mktemp -d /tmp/.crowbar-tmp-XXXXXXX)

# Location for caches that should not be erased between runs
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.crowbar-build-cache"

export CROWBAR_TMP CACHE_DIR
# We might use lots and lots of open files.  Bump our open FD limits.
ulimit -Sn unlimited

# Hashes to hold our "interesting" information.
# Key = barclamp name
# Value = whatever interesting thing we are looking for.
[[ ${BC_QUERY_STRINGS[*]} ]] || declare -A BC_QUERY_STRINGS

declare -A BC_DEPS BC_GROUPS BC_PKGS BC_EXTRA_FILES BC_OS_SUPPORT BC_GEMS
declare -A BC_REPOS BC_PPAS BC_RAW_PKGS BC_BUILD_PKGS
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
declare -A BC_DEPS BC_GROUPS BC_PKGS BC_EXTRA_FILES BC_OS_SUPPORT BC_GEMS
declare -A BC_REPOS BC_PPAS BC_RAW_PKGS BC_BUILD_PKGS
declare -A BC_SMOKETEST_DEPS BC_SMOKETEST_TIMEOUTS BC_BUILD_CMDS
declare -A BC_SUPERCEDES BC_SRC_PKGS

GEM_EXT_RE='^(.*)-\((.*)\)$'

extract_barclamp_metadata() {
    # $1 = path to barclamp
    # $2 = git commit-ish to extract metadata from
    # Returns path to extracted crowbar.yml.
    local mode type sha name
    [[ -d $CROWBAR_DIR/barclamps/$1/.git || \
        -f $CROWBAR_DIR/barclamps/$1/.git ]] || \
        die "$1 is not a barclamp/."
    if [[ $2 ]]; then
        read mode type sha name < <(cd "$CROWBAR_DIR/barclamps/$1"; git ls-tree "$2" crowbar.yml)
        [[ $name ]] || return
        [[ -f $CROWBAR_TMP/$sha.yml ]] || \
            (cd "$CROWBAR_DIR/barclamps/$1"; git cat-file "$type" "$sha") > "$CROWBAR_TMP/$sha.yml"
        echo "$CROWBAR_TMP/$sha.yml"
    else
        echo "$CROWBAR_DIR/barclamps/$1/crowbar.yml"
    fi
}

read_barclamp_metadata() {
    # $1 = path to the .yml with the metadata.
    # $@ = args to pass to parse_yml.rb.
    local yml_file
    [[ -f $1 && $1 = *.yml ]] || die "$1 is not a YML file."
    yml_file="$1"
    shift
    "$CROWBAR_DIR/parse_yml.rb" "$yml_file" "$@" 2>/dev/null
}

get_one_barclamp_info() {
    # $1 = barclamp name
    # $2 = git commit-ish
    # Gets all the info that BC_QUERY_STRINGS wants.
    [[ -d $CROWBAR_DIR/barclamps/$1 ]] || die "$1 is not a barclamp!"
    local mdfile="$CROWBAR_DIR/barclamps/$1/crowbar.yml"
    local query line
    [[ $mdfile ]] || return
    [[ $1 = crowbar ]] || BC_DEPS["$1"]+="crowbar "
    for query in "${!BC_QUERY_STRINGS[@]}"; do
        while read line; do
            [[ $line = nil ]] && continue
            case $query in
                deps) is_in "$line "${BC_DEPS["$1"]} || \
                    BC_DEPS["$1"]+="$line ";;
                groups) is_in "$line" ${BC_GROUPS["$1"]} ||
                    BC_GROUPS["$line"]+="$1 ";;
                pkgs|os_pkgs) is_in "$line" ${BC_PKGS["$1"]} || \
                    BC_PKGS["$1"]+="$line ";;
                src_pkgs) is_in "$line" ${BC_SRC_PKGS["$1"]} || \
                    BC_SRC_PKGS["$1"]+="$line ";;
                extra_files) BC_EXTRA_FILES["$1"]+="$line\n";;
                os_support) BC_OS_SUPPORT["$1"]+="$line ";;
                gems) BC_GEMS["$1"]+="$line ";;
                repos|os_repos) BC_REPOS["$1"]+="$line\n";;
                ppas|os_ppas) [[ $PKG_TYPE = debs ]] || \
                    die "Cannot declare a PPA for $PKG_TYPE!"
                    BC_REPOS["$1"]+="ppa $line\n";;
                build_pkgs|os_build_pkgs) BC_BUILD_PKGS["$1"]+="$line ";;
                raw_pkgs|os_raw_pkgs|pkg_sources|os_pkg_sources) BC_RAW_PKGS["$1"]+="$line ";;
                test_deps) BC_SMOKETEST_DEPS["$1"]+="$line ";;
                os_build_cmd|build_cmd) [[ ${BC_BUILD_CMDS["$1"]} ]] || \
                    BC_BUILD_CMDS["$1"]="$line";;
                test_timeouts) BC_SMOKETEST_TIMEOUTS["$1"]+="$line ";;
                supercedes)
                    [[ ${BC_SUPERCEDES[$line]} ]] || \
                    BC_SUPERCEDES["$line"]="$1"
                    debug "${BC_SUPERCEDES[$line]} supercedes $line";;
                *) die "Cannot handle query for $query."
            esac
        done < <(read_barclamp_metadata "$mdfile" ${BC_QUERY_STRINGS["$query"]})
    done
}

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
        get_one_barclamp_info "$bc"
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

flat_checkout() [[ -d $CROWBAR_DIR/releases ]]

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
    # Nuke any wild caches.
    if [[ $WILD_CACHE = true ]]; then
        for f in "${CACHE_DIR%/*}/.crowbar_temp_cache"*; do
            [[ -d $f ]] || continue
            sudo rm -rf "$f"
        done
    fi
    if [[ -d $CACHE_DIR && -d $CACHE_DIR/.git && $MAYBE_UPDATE_GIT_CACHE ]]; then
        cd "$CACHE_DIR"
        if ! in_cache git diff-index --cached --quiet HEAD; then
            in_cache git commit -m "Updated by build_crowbar.sh @ $(date) for ${OS_TOKEN}"
            echo "The crowbar build cache has been updated, and the updates have"
            echo "been comitted back to the cache.  Please push any changes."
        fi
    fi
    wait
    flock -u 70
    rm "$CROWBAR_DIR/".*.lock
    [[ -d $CROWBAR_TMP ]] && rm -rf "$CROWBAR_TMP"
    exit $res
} 70> "$CLEANUP_LOCK"

# Arrange for cleanup to be called at the most common exit points.
trap cleanup EXIT INT QUIT TERM
[[ $DEBUG ]] && trap -p

# Test to see if $1 is in the rest of the args.
is_in() {
    local t="(^| )$1( |\$)"
    shift
    [[ $* =~ $t ]]
}

# Run a command in our chroot environment.
in_chroot() { sudo -H chroot "$CHROOT" /bin/bash -l -c "$*"; }

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
    if [[ $http_proxy || $https_proxy || $no_proxy ]]; then
        local __f
        __f=$(mktemp /tmp/proxy-XXXXX.sh) || \
            die "Canot make utility chroot -- error adding proxies."
        [[ $http_proxy ]] && echo "http_proxy=\"$http_proxy\"" >> "$__f"
        [[ $https_proxy ]] && echo "https_proxy=\"$https_proxy\"" >> "$__f"
        [[ $no_proxy ]] && echo "no_proxy=\"$no_proxy\"" >> "$__f"
        sudo cp "$__f" "$CHROOT/etc/environment"
        [[ $http_proxy ]] && echo "export http_proxy" >> "$__f"
        [[ $https_proxy ]] && echo "export https_proxy" >> "$__f"
        [[ $no_proxy ]] && echo "export no_proxy" >> "$__f"
        sudo cp "$__f" "$CHROOT/etc/profile.d/proxy.sh"
    fi
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
    if [[ $CURRENT_CACHE_BRANCH ]]; then
        CACHE_NEEDS_COMMIT=true
        in_cache git add "${2#${CACHE_DIR}/}"
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
    sudo mount --bind "$(readlink -f "$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs")" \
        "$CHROOT/mnt"
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

# Fetch all of a gem's dependencies, followed by the gem itself.
__fetch_gem() {
    # $1 = gem to fetch.
    # $2 = (optional) version string
    if [[ ${fetched_gems["$1 $2"]} ]]; then
        debug "Gem ${fetched_gems["$1 $2"]} already fetched to satisfy $1 $2"
        return
    fi
    local gemver_re='(.+) \(([^)]*)\)'
    local our_versions=() v=()
    local -A gem_versions
    local fetch_gems=()
    local in_our_gem=false line gemname i our_gemname
    IFS=',' read -a v <<< "${2}"
    # Translate into something we can pass to our gem wrapper.
    for i in "${v[@]}"; do
        our_versions+=(--version "$i")
    done
    # Download the gem that best matches the constraints
    our_gemname=$("$CROWBAR_DIR/fetch_all_gems.rb" fetch "$1" "${our_versions[@]}")
    our_gemname=${our_gemname##*Downloaded }
    # Figure out the version we downloaded
    [[ $our_gemname =~ $GEM_RE ]] || die "Could not find version of downloaded gem!"
    fetched_gems["$1 $2"]="$our_gemname"
    # and get the dependencies for gems that match these constraints.
    our_versions=(--version "${BASH_REMATCH[2]}")
    while read line; do
        case $line in
            Gem*)
                in_our_gem=false
                gemname=${line#Gem }
                [[ $gemname = $our_gemname ]] || continue
                in_our_gem=true;;
            '') [[ $in_our_gem = true ]] && break
                gemname='';;
            *) [[ $in_our_gem = true && \
                $line =~ $gemver_re && \
                ${BASH_REMATCH[2]} != *development* ]] || continue
                fetch_gems+=("${BASH_REMATCH[1]}")
                gem_versions["${BASH_REMATCH[1]}"]="${BASH_REMATCH[2]}";;
        esac
    done < <("$CROWBAR_DIR/fetch_all_gems.rb" dependency "$1" --remote "${our_versions[@]}")
    for gemname in "${fetch_gems[@]}"; do
        [[ ${fetched_gems["$gemname ${gem_versions[$gemname]}"]} ]] && continue
        debug "Fetching $gemname ${gem_versions[$gemname]} as a dependency of $1"
        __fetch_gem "$gemname" "${gem_versions[$gemname]}"
    done
}

# Update the gem cache for a barclamp
update_barclamp_gem_cache() {
    local -A gems
    local gemname gemver gemopts bc gem
    local bc_cache="$CACHE_DIR/barclamps/$1/gems"
    which gem &>/dev/null || die "Please install rubygems before updating the gem cache!"
    local gemdir="$CROWBAR_TMP/gems/$1"
    mkdir -p "$gemdir"

    # Stage prerequisite gems first
    for bc in $(all_deps "$1"); do
        [[ -d "$CACHE_DIR/barclamps/$bc/gems/." ]] && \
            cp -a "$CACHE_DIR/barclamps/$bc/gems/." \
            "$gemdir"
    done

    # Remember what we already have.
    while read gem; do
        gems["${gem##*/}"]="true"
    done < <(find "$gemdir" -type f -name '*.gem')

    (   cd "$gemdir"
        local -A fetched_gems
        for gem in ${BC_GEMS["$1"]}; do
            debug "Fetching top-level gem $gem"
            if [[ $gem =~ $GEM_RE ]]; then
                __fetch_gem "${BASH_REMATCH[1]}" "= ${BASH_REMATCH[2]}"
            elif [[ $gem =~ $GEM_EXT_RE ]]; then
                __fetch_gem "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
            else
                __fetch_gem "$gem" ">= 0"
            fi
        done
    ) || exit 1
    mkdir -p "$bc_cache" || die "Failed to make gem cache ${bc_cache}"
    while read gem; do
        [[ ${gems["${gem##*/}"]} = "true" ]] && continue
        cache_add "$gem" "$bc_cache"
    done < <(find "$gemdir" -type f -name '*.gem')
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
    make_barclamp_pkg_metadata "$1"
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
    local pkg pkgname arch bcs=() bc ret=1
    local -A pkgs
    [[ ${BC_PKGS["$1"]} || ${BC_BUILD_PKGS["$1"]} ]] || return 1
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
        debug "Package $pkg is not cached, and $1 needs it."
        ret=0
    done
    return $ret
}

# Check to see if the barclamp gem cache needs an update.
barclamp_gem_cache_needs_update() {
    local pkg pkgname bc ret=1
    local -A pkgs
    # Second, check to see if we have all the gems we need.
    for pkg in ${BC_GEMS["$1"]}; do
        if [[ $pkg =~ $GEM_RE ]]; then
            local finder="$pkg.gem"
        elif [[ $pkg =~ $GEM_EXT_RE ]]; then
            local finder="${BASH_REMATCH[1]}*.gem"
        else
            local finder="$pkg*.gem"
        fi
        for bc in $(all_deps "$1"); do
            local bc_cache="$CACHE_DIR/barclamps/$bc/gems"
            mkdir -p "$bc_cache"
            [[ $(find "$bc_cache" \
                -name "$finder" -type f) = *.gem ]] && continue 2
        done
        debug "Gem $pkg is not cached, and $1 needs it."
        ret=0
    done
    return $ret
}

# CHeck to see if we are missing any raw packages.
barclamp_raw_pkg_cache_needs_update() {
    local pkg bc_cache="$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs" ret=1
    [[ ${BC_RAW_PKGS["$1"]} || ${BC_PKG_SOURCES["$1"]} ]] || return 1
    mkdir -p "$bc_cache"
    # Third, check to see if we have all the raw_pkgs we need.
    for pkg in ${BC_RAW_PKGS["$1"]} ${BC_PKG_SOURCES["$1"]}; do
        [[ -f $bc_cache/${pkg##*/} ]] && continue
        debug "Raw package $pkg is not cached, and $1 needs it."
        ret=0
    done
    return $ret
}

# Check to see if we are missing any raw files.
barclamp_file_cache_needs_update() {
    local pkg dest bc_cache="$CACHE_DIR/barclamps/$1/files" ret=1
    [[ "${BC_EXTRA_FILES[$1]}" ]] || return 1
    mkdir -p "$bc_cache"
    # Fourth, check to make sure we have all the extra_pkgs we need.
    while read pkg; do
        dest=${pkg#* }
        [[ $dest = $pkg ]] && dest=''
        pkg=${pkg%% *}
        [[ -f $bc_cache/$dest/${pkg##*/} ]] && continue
        debug "File $pkg is not cached, and $1 needs it."
        ret=0
    done < <(write_lines "${BC_EXTRA_FILES[$1]}")
    return $ret
}

# Some helper functions

log() { printf "$(date '+%F %T %z'): %s\n" "$@" >&2; }

warn() { log "$@"; }

# Print a message to stderr and exit.  cleanup will be called.
die() { log "$@" >&2; res=1; exit 1; }

# Print a message to stderr and keep going.
debug() { [[ $VERBOSE ]] && log "$@"; }

# Clean up any cruft that we might have left behind from the last run.
clean_dirs() {
    local d=''
    for d in "$@"; do
        (   mkdir -p "$d"
            cd "$d"
            sudo rm -rf --one-file-system * )
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
is_barclamp() [[ -f "$CROWBAR_DIR/barclamps/$1/crowbar.yml" ]]
in_barclamp() {
    (   cd "$CROWBAR_DIR/barclamps/$1"
        shift
        "$@")
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

get_repo_cfg() { in_repo git config --get "$1"; }
git_config_has() { git config --get "$1" &>/dev/null; }
current_build() { get_repo_cfg 'crowbar.build'; }
build_exists() [[ -f $CROWBAR_DIR/releases/$1/barclamp-crowbar || \
    -L $CROWBAR_DIR/releases/$1/parent ]]

__barclamp_exists_in_build() {
    local build=${1%/*} bc=${1##*/}
    [[ -f $CROWBAR_DIR/releases/$build/barclamp-$bc ]]
}

barclamp_exists_in_build() {
    __barclamp_exists_in_build "$1" && return 0
    local build=${1%/*} bc=${1##*/}
    [[ -L $CROWBAR_DIR/releases/$build/parent ]] || return 1
    local r=$(readlink "$CROWBAR_DIR/releases/$build/parent")
    r=${r##*/}
    barclamp_exists_in_build "${build%/*}/$r/$bc"
}

build_cfg_dir() {
    local d="${1:-$(current_build)}"
    build_exists "$d" || return 1
    echo "$CROWBAR_DIR/releases/$d"
}

release_exists() [[ -d $CROWBAR_DIR/releases/$1/master ]]

# Get the current release we are working on, which is a function of
# the currently checked-out branch.
current_release() {
    local rel
    rel=$(current_build) || \
	die "current_release: Cannot get current build information!"
    echo "${rel%/*}"
}

release_cfg_dir() {
    local d="${1:-$(current_release)}"
    release_exists "$d" || return 1
    echo "$CROWBAR_DIR/releases/$d"
}

# Find all barclamps for whatever.
barclamp_finder() {
    # $1 = directory under $CROWBAR_DIR/releases to look in.
    # $2 = regex to use as a filter.
    # $2 = Match in the RE to return.  Defaults to 1
    local b
    while read b; do
	[[ $b =~ $2 ]] || continue
	printf '%s\n' "${BASH_REMATCH[${3:-1}]}"
    done < <(find "$CROWBAR_DIR/releases/$1" -name 'barclamp-*' -or -name 'parent') |sort -u
}

builds_for_barclamp_in_release() {
    # $1 = barclamp
    # $2 = release
    release_exists "$2" || die "No such release $2!"
    barclamp_finder "$2" "releases/.+/([^/]+)/barclamp-$1"
}

barclamps_from_build() {
    flat_checkout || die "Cannot get list of barclamps, must flatten build first!"
    local build bc
    build="${1:-$(current_build)}"
    barclamp_finder "$build" '/barclamp-(.+)$'
}

parent_build() {
    build_exists "$1" || return 1
    [[ -L $CROWBAR_DIR/releases/$1/parent ]] || return 1
    local p
    p="$(readlink -f "$CROWBAR_DIR/releases/$1/parent")"
    echo "${p##*releases/}"
}

# Get or set the proper branch for a barclamp for a build.
barclamp_branch_for_build() {
    # $1 = build
    # $2 = barclamp
    # $3 = (optional) ref to pin the barclamp at.
    local build=$1 bcfile
    while [[ true ]]; do
        __barclamp_exists_in_build "$build/$2" && break
        build=$(parent_build "$build") || break
    done
    if [[ ! $3 ]]; then
        if [[ $build ]]; then
            cat "$CROWBAR_DIR/releases/$build/barclamp-$2"
        else
            echo "empty-branch"
        fi
	return 0
    elif [[ $build ]]; then
        bcfile="$CROWBAR_DIR/releases/$build/barclamp-$2"
        [[ -f $bcfile ]] && \
	    in_barclamp "$2" git rev-parse --verify --quiet "$3" &>/dev/null || return 1
        echo "$3" > "$bcfile"
        git add "$bcfile"
    else
        return 1
    fi
}

barclamps_in_build() {
    local build bc p
    build="${1:-$(current_build)}"
    p="$(parent_build "$build")"
    [[ $p ]] && barclamps_in_build "$p"
    barclamps_from_build "$build"
}

barclamps_in_release() {
    local release="${1:-$(current_release)}"
    release_exists "$release" || return 1
    barclamp_finder "$release" '/barclamp-(.+)$'
}

builds_in_release() {
    local release="${1:-$(current_release)}" p build b
    local -A builds
    release_exists "$release" || return 1
    for build in $(barclamp_finder "$release" "releases/.+/([^/]+)/(barclamp-crowbar|parent)$"); do
        build_exists "$release/$build" || continue
        p=$(parent_build "$release/$build")
        if [[ $p && ${builds[$p]} != echoed  ]]; then
            builds["$release/$build"]="$p"
        else
            echo "$build"
            builds["$release/$build"]="echoed"
        fi
    done
    while [[ true ]]; do
        b=true
        for build in "${!builds[@]}"; do
            p="${builds[$build]}"
            [[ $p = echoed || ${builds[$p]} != echoed ]] && continue
            echo "${build##*/}"
            builds[$build]=echoed
            b=false
        done
        [[ $b = true ]] && break
    done
}

all_barclamps() {
    local bc
    local -A barclamps
    barclamp_finder '' '/barclamp-(.+)$'
}

# Given a build, give us the branch the barclamps will use.
build_branch() {
    # $1 = build
    case $1 in
	development/*) echo "master" ;;
	stable/*) echo "stable/${1%/*}/master";;
	feature/*/*) echo "${1%/*}/master";;
	*) echo "release/${1%/*}/master";;
    esac
}

# Given a branch, figure out what release it is for
release_for_branch() {
    # $1 = branch name
    local r
    case $1 in
        master) echo "development";;
        feature/*) echo "${1%/*}";;
        release/*) r="${1#release/}"; echo "${r%/*}";;
    esac
}

crowbar_version() {
    local bc build br
    build=${1:-$(current_build)}
    commits=0
    build_exists "$build" || die "$build is not a build!"
    for bc in $(barclamps_in_build); do
        br="$(barclamp_branch_for_build "$build" "$bc")"
        commits=$((commits + $(in_barclamp "$bc" git rev-list --count --sparse --no-merges "$br")))
    done
    echo "${build//\//_}.$commits"
}

all_releases() { barclamp_finder '' 'releases/(.+)/master/barclamp-crowbar$'; }

all_supported_oses() {
    local os
    for os in "$CROWBAR_DIR/"*-extra; do
        [[ -f $os/build_lib.sh ]] || continue
        os=${os##*/}
        echo "${os%-extra}"
    done
}

if [[ $http_proxy ]]; then
    export USE_PROXY=1
    raw_proxy="${http_proxy#*://}"
    raw_proxy="${raw_proxy%/}"
    proxy_re='^(.+):([0-9]+)$'
    hostsplit_re='(.*)@(.*)'
    userpass_re='(.*):(.*)'
    if [[ $raw_proxy =~ $proxy_re ]]; then
        export PROXY_PORT="${BASH_REMATCH[2]}"
        raw_proxy="${BASH_REMATCH[1]}"
    fi
    if [[ $raw_proxy =~ $hostsplit_re ]]; then
        raw_proxy="${BASH_REMATCH[2]}"
        export PROXY_USER="${BASH_REMATCH[1]}"
        if [[ ${BASH_REMATCH[1]} =~ $userpass_re ]]; then
            export PROXY_PASSWORD="${BASH_REMATCH[2]}"
            export PROXY_USER="${BASH_REMATCH[1]}"
        fi
    fi
    export PROXY_HOST="$raw_proxy"
    [[ $no_proxy ]] || no_proxy="localhost,localhost.localdomain,127.0.0.0/8,$PROXY_HOST"
else
    unset USE_PROXY PROXY_HOST PROXY_PORT PROXY_USER PROXY_PASS
fi

export PATH="$PATH:$CROWBAR_DIR:$CROWBAR_DIR/extra:$CROWBAR_DIR/change-image/dell:$CROWBAR_DIR/test_framework"

CROWBAR_BUILD_SOURCED=true
