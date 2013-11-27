#!/bin/bash
# This contains library routines for handling git tracked metadata.

# Helper for running commands in the metadata repository.
in_meta() ( cd "$CROWBAR_DIR/.releases" && "$@" )
meta_file_probe() {
    in_meta branch_exists "$1" || return 1
    local mode type sha name
    read mode type sha name < <(in_meta git ls-tree --full-tree -r "$1" "$2")
    [[ $sha && $type = blob ]]
}

# git_cat_file, but specialized for working in the metadata repo.
meta_cat_file() {
    meta_file_probe "$1" "$2" || return 1
    git_cat_file "$CROWBAR_DIR/.releases" "$1" "$2"
}

# Test it see if $1 is a release.
release_exists() {
    meta_file_probe "$1" master/barclamp-crowbar
}

# test to see if $1 is a build.
build_exists() {
    local build=${1##*/} rel=${1%/*}
    meta_file_probe "$rel" "$build/parent" || \
        meta_file_probe "$rel" "$build/barclamp-crowbar"
}

__barclamp_exists_in_build() {
    local bc="${1##*/}" __buildparts="${1%/*}"
    local build="${__buildparts##*/}" rel="${__buildparts%/*}"
    meta_file_probe "$rel" "$build/barclamp-$bc"
}

build_cfg_dir() {
    local d="${1:-$(current_build)}"
    build_exists "$d" || return 1
    echo "$CROWBAR_DIR/.releases/${d##*/}"
}

release_cfg_dir() {
    local d="${1:-$(current_release)}"
    release_exists "$d" || return 1
    echo "$CROWBAR_DIR/.releases"
}

# Find all barclamps matching a given RE in a specific release.
# Internal to git_tracked_metadata.sh
barclamp_finder() {
    # $1 = release to look in.
    # $2 = Regex to match against.
    # $3 = Match in the RE to return.  Defaults to 1.
    local name_re='^[^/]+/(parent|barclamp-.+)$'
    local mode type sha name
    while read mode type sha name; do
        [[ $sha && $type = blob && $name =~ $name_re ]] || continue
        [[ $name =~ $2 ]] || continue
        printf "%s\n" "${BASH_REMATCH[${3:-1}]}"
    done < <(in_meta git ls-tree --full-tree -r "$1") |sort -u
}

builds_for_barclamp_in_release() {
    release_exists "$2" || die "No such release $2!"
    barclamp_finder "$2" "([^/]+)/barclamp-$1"
}

barclamps_from_build() {
    local d="${1:-$(current_build)}"
    local build="${d##*/}" rel="${d%/*}"
    barclamp_finder "$rel" "$build/barclamp-(.+)\$"
}

parent_build() {
    build_exists "$1" || return 1
    local build="${1##*/}" rel="${1%/*}" parent
    parent=$(meta_cat_file "$rel" "$build/parent") || return 1
    echo "$rel/${parent##*/}"
}

get_barclamp_branch_for_build() {
    local build="${1##*/}" rel="${1%/*}"
    meta_cat_file "$rel" "$build/barclamp-$2"
}

set_barclamp_branch_for_build() (
    local build="${1##*/}" rel="${1%/*}" res=0
    cd "$CROWBAR_DIR/.releases"
    quiet_checkout "$rel" || return 1
    local bcfile="$build/barclamp-$2"
    echo "$3" > "$bcfile"
    git add "$bcfile"
)

barclamps_in_release() {
    local release="${1:-$(current_release)}"
    release_exists "$release" || return 1
    barclamp_finder "$release" '/barclamp-(.+)$'
}

__builds_in_release() {
    barclamp_finder "$1" '^([^/]+)/(barclamp-crowbar|parent)$'
}

all_releases() {
    local sha ref
    while read sha ref; do
        [[ $ref = refs/heads/* ]] || continue
        ref=${ref#refs/heads/}
        release_exists "$ref" || continue
        printf "%s\n" "$ref"
    done < <(in_meta git show-ref --heads)
}

all_barclamps() {
    local rel
    while read rel; do
        barclamps_in_release "$rel"
    done < <(all_releases) |sort -u
}

__switch_release_helper() {
    # $1 = the build we are switching to.
    local l build="${1##*/}" rel="${1%/*}"
    in_repo quiet_checkout master
    in_meta quiet_checkout "$rel"
    for l in change-image extra; do
        [[ $(in_repo readlink -f $l) = ".releases/$build/$l" ]] && continue
        in_repo rm -f "$l"
        in_repo ln -sf ".releases/$build/$l" "$l"
    done
}

__release_update() { in_meta git commit -m "${1:-Updating release metadata}"; }

__release_cleanup() { in_meta git reset --hard; }

parent_release() {
    release="${1:-$(current_release)}"
    meta_cat_file "$release" "parent"
}

set_parent_release() (
    release_exists "$1" || die "Release $1 does not exist, cannot set its parent!"
    cd "$CROWBAR_DIR/.releases"
    local head=$(get_current_head)
    quiet_checkout "$1"
    echo "$2" > parent
    git add parent
    git commit -m "Setting parent of $1 to $2"
    quiet_checkout "$head"
)

clone_release(){
    in_meta git branch -f "$2" "$1"
    in_meta quiet_checkout "$2"
}

__kill_release() (
    cd "$CROWBAR_DIR/.releases"
    quiet_checkout master
    git branch -D "$1"
)

# The next two assume that you already have the metadata checked out to the
# proper release.
__add_barclamp_to_build() (
    cd "$CROWBAR_DIR/.releases"
    local build="${1##*/}" rel="${1%/*}"
    echo "$3" > "$build/barclamp-$2"
    git add "$build/barclamp-$2"
)

__kill_build() {
    local build="${1##*/}" rel="${1%/*}"
    in_meta git rm -rf "$build"
    in_meta rm -rf "$build"
}
