#!/bin/bash
# This contains library routines for handling flat metadata.

# Test to see if a build exists.
build_exists() [[ -f $CROWBAR_DIR/releases/$1/barclamp-crowbar || \
    -L $CROWBAR_DIR/releases/$1/parent ]]

__barclamp_exists_in_build() {
    local build=${1%/*} bc=${1##*/}
    [[ -f $CROWBAR_DIR/releases/$build/barclamp-$bc ]]
}

# Given a fully specced build, point at its config metadata.
build_cfg_dir() {
    local d="${1:-$(current_build)}"
    build_exists "$d" || return 1
    echo "$CROWBAR_DIR/releases/$d"
}

## Test to see if a given release exists.
release_exists() [[ -d $CROWBAR_DIR/releases/$1/master ]]

# Given a release, return its config dir.
release_cfg_dir() {
    local d="${1:-$(current_release)}"
    release_exists "$d" || return 1
    echo "$CROWBAR_DIR/releases/$d"
}

# Find all barclamps for whatever.
# Utility to be used internally in flat_metadata.sh.
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

# Given a barclamp name and a release, see what builds in that release use the barclamp.
builds_for_barclamp_in_release() {
    # $1 = barclamp
    # $2 = release
    release_exists "$2" || die "No such release $2!"
    barclamp_finder "$2" "releases/.+/([^/]+)/barclamp-$1"
}

# Given a build, see what barclamps it explicitly lists.
barclamps_from_build() {
    flat_checkout || die "Cannot get list of barclamps, must flatten build first!"
    local build bc
    build="${1:-$(current_build)}"
    barclamp_finder "$build" '/barclamp-(.+)$'
}

# Given a build, see what its parent is.
parent_build() {
    build_exists "$1" || return 1
    [[ -L $CROWBAR_DIR/releases/$1/parent ]] || return 1
    local p
    p="$(readlink -f "$CROWBAR_DIR/releases/$1/parent")"
    echo "${p##*releases/}"
}

# Get the branch that a barclamp should be checked out to for
# performing a build.
get_barclamp_branch_for_build() {
    # $1 = build
    # $2 = barclamp
    cat "$CROWBAR_DIR/releases/$1/barclamp-$2"
}

# Set the branch that a barclamp should be checked out to for
# performing a build.
set_barclamp_branch_for_build() {
    # $1 = build
    # $2 = barclamp
    # $3 = commit-ish to pin the barclamp at
    bcfile="$CROWBAR_DIR/releases/$1/barclamp-$2"
    [[ -f $bcfile ]] || return 1
    echo "$3" > "$bcfile"
    in_repo git add "$bcfile"
}

# Given a release, return the set of all barclamps that are mentioned for it.
barclamps_in_release() {
    local release="${1:-$(current_release)}"
    release_exists "$release" || return 1
    barclamp_finder "$release" '/barclamp-(.+)$'
}

__builds_in_release() {
    barclamp_finder "$1" "releases/.+/([^/]+)/(barclamp-crowbar|parent)\$"
}

# Return all the barclamps that are members of any release.
all_barclamps() {
    local bc
    local -A barclamps
    barclamp_finder '' '/barclamp-(.+)$'
}

# Return all of the releases.
all_releases() { barclamp_finder '' 'releases/(.+)/master/barclamp-crowbar$'; }

# Helper for switching releases.  Takes care of any metadata-specific
# tasks that must be performed on a switch.
__switch_release_helper() {
    # $1 = the build we are switching to.
    in_repo quiet_checkout master
    local l
    for l in change-image extra; do
        [[ $(in_repo readlink -f $l) = "releases/$1/$l" ]] && continue
        in_repo rm -f "$l"
        in_repo ln -sf "releases/$1/$l" "$l"
    done
}

# Get the parent release for the current release
parent_release() {
    release="${1:-$(current_release)}"
    [[ -f $CROWBAR_DIR/releases/$release/parent ]] || return 1
    cat "$CROWBAR_DIR/releases/$release/parent"
}

# Set release $2 to be the parent of $1
set_parent_release() {
    echo "$2" > "$CROWBAR_DIR/releases/$1/parent"
    in_repo git commit -m "Making $2 the parent release for $1" "releases/$1/parent"
}

# Performs any metadata-specific tasks that are needed to commit a
# release metadata change.
__release_update() {
    in_repo git commit -m "${1:-Updating release metadata}" releases
}

# Performs any metadata-specific tasks that are needed to clean up
# after a failed pin_release.
__release_cleanup() {
    in_repo git rm -r --cached releases/
    in_repo git checkout HEAD -- releases
}

# Clone release $1 to $2
clone_release() {
    mkdir -p "$CROWBAR_DIR/releases/$2"
    cp -ap "$CROWBAR_DIR/releases/$1/." \
        "$CROWBAR_DIR/releases/$2/."
    in_repo git add "releases/$2/"
}

# Kill metadata for a release.  Should only be called by erase_release,
__kill_release() {
    # $1 = release to kill.
    in_repo git rm -rf "releases/$1"
    in_repo rm -rf "releases/$1"
    in_repo git commit -m "Erasing release $1"
}

# Add a barclamp to a build.
__add_barclamp_to_build() {
    # $1 = build
    # $2 = barclamp
    # $3 = commit-ish to use for barclamp in this build.
    echo "$3" > "$CROWBAR_DIR/releases/$1/barclamp-$2"
    in_repo git add "releases/$1/barclamp-$2"
}

# Kill a build.
__kill_build() {
    in_repo git rm -rf "releases/$1"
    in_repo rm -rf "releases/$1"
}
