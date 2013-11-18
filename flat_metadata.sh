#!/bin/bash

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

all_releases() { barclamp_finder '' 'releases/(.+)/master/barclamp-crowbar$'; }
