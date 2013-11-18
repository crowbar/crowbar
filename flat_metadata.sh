#!/bin/bash

# Test to see if a build exists.
build_exists() [[ -f $CROWBAR_DIR/releases/$1/barclamp-crowbar || \
    -L $CROWBAR_DIR/releases/$1/parent ]]

__barclamp_exists_in_build() {
    local build=${1%/*} bc=${1##*/}
    [[ -f $CROWBAR_DIR/releases/$build/barclamp-$bc ]]
}

# Test to see if a barclamp is part of a specific build.
barclamp_exists_in_build() {
    __barclamp_exists_in_build "$1" && return 0
    local build=${1%/*} bc=${1##*/}
    [[ -L $CROWBAR_DIR/releases/$build/parent ]] || return 1
    local r=$(readlink "$CROWBAR_DIR/releases/$build/parent")
    r=${r##*/}
    barclamp_exists_in_build "${build%/*}/$r/$bc"
}

# Given a fully specced build, point at its config metadata.
build_cfg_dir() {
    local d="${1:-$(current_build)}"
    build_exists "$d" || return 1
    echo "$CROWBAR_DIR/releases/$d"
}

## Test to see if a given release exists.
release_exists() [[ -d $CROWBAR_DIR/releases/$1/master ]]

# Get the current release we are working on, which is a function of
# the currently checked-out branch.
current_release() {
    local rel
    rel=$(current_build) || \
        die "current_release: Cannot get current build information!"
    echo "${rel%/*}"
}

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

# Given a build, see what barclamps will go into it.
barclamps_in_build() {
    local build bc p
    build="${1:-$(current_build)}"
    p="$(parent_build "$build")"
    [[ $p ]] && barclamps_in_build "$p"
    barclamps_from_build "$build"
}

# Given a release, return the set of all barclamps that are mentioned for it.
barclamps_in_release() {
    local release="${1:-$(current_release)}"
    release_exists "$release" || return 1
    barclamp_finder "$release" '/barclamp-(.+)$'
}

# Given a release, find all the builds that are in it.
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

# Return all the barclamps that a members of any release.
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

# Performs any metadata-specific tasks that are needed to clean up
# after a failed pin_release.
__pin_release_cleanup() {
    in_repo git rm -r --cached releases/
    in_repo git checkout HEAD -- releases
}

###
# The functions below are lifted straight out of the dev tool.
# They will need to be refactored into devtool-specific and metadata-specific
# parts once I start writing the git-tracked-metadata library.
### 

# Given a release, find the "best" parent release.  This will only
# be called if we don't already have metadata recorded for the
# parent relationship of this release.
find_best_parent() {
    # $1 = release to find the "best" parent of.
    #      If empty, use the release we are currently on.
    local br distance best_distance ref candidate merge_base release
    local best_candidates=()
    if [[ $1 ]]; then
        release_exists "$1" || \
            die "find_best_parent: $1 is not a release"
        release="$1"
    else
        release="$(current_release)"
    fi
    if [[ -f $CROWBAR_DIR/releases/$release/parent ]]; then
        cat "$CROWBAR_DIR/releases/$release/parent"
        return 0
    elif [[ $release != feature/* ]]; then
        echo "$release"
        return 0
    elif in_repo git_config_has "crowbar.releases.$release.parent"; then
        get_repo_cfg "crowbar.releases.$release.parent" | \
            tee "$CROWBAR_DIR/releases/$release/parent"
        in_repo git config --unset "crowbar.releases.$release.parent"
        in_repo git commit -m "Adding parent for $release" "releases/$release/parent"
        return 0
    fi
    debug "More than one good candidate for a parent of $release found."
    debug "Please pick the one you want:"
    select candidate in $(all_releases) "None of the above"; do
        case $candidate in
            'None of the above') die "Aborting.";;
            '') continue;;
            *) break;;
        esac
    done
    echo "$candidate" > "$CROWBAR_DIR/releases/$release/parent"
    in_repo git commit -m "Adding parent for $release" "releases/$release/parent"
}


# Create a new release branch structure based on the current state of the
# Crowbar repositories.
cut_release() {
    local new_branch bc

    [[ $1 ]] || die "cut_release: Please specify a name for the new release"

    # Test to see if release exists.
    release_exists "$1" && die "cut_release: Name already exists"
    local can_cut=true
    new_branch="$(release_branch $1)"
    current_release=$(current_release)
    barclamps_are_clean || \
        die "Crowbar repo must be clean before trying to cut a release!"

    mkdir -p "$CROWBAR_DIR/releases/$1"
    cp -ap "$CROWBAR_DIR/releases/$current_release/." \
        "$CROWBAR_DIR/releases/$1/."
    for build in $(builds_in_release "$current_release"); do
        debug "Creating build $build in new release $1"
        for bc in $(barclamps_from_build "$current_release/$build"); do
            br="$(barclamp_branch_for_build "$current_release/$build" "$bc")"
            [[ $br && $br != empty-branch ]] || continue
            in_barclamp "$bc" git branch -f --no-track "$new_branch" "$br"
            echo "$new_branch" > "$CROWBAR_DIR/releases/$1/$build/barclamp-$bc"
        done
    done
    [[ $1 = feature/* ]] && \
        echo "$(current_release)" > "$CROWBAR_DIR/releases/$1/parent"
    {   in_repo git add "releases/$1"
        in_repo git commit -m "Added metadata for new release $1"
    } &>/dev/null
    if git_managed_cache && ! in_cache branch_exists "$new_branch"; then
        debug "Creating $new_branch for $1 in the build cache"
        if in_cache branch_exists "$(release_branch)"; then
            in_cache git branch "$new_branch" "$(release_branch)"
        else
            in_cache git branch "$new_branch" master
        fi
    fi
    debug "$1 created."
    debug "You can switch to it with $0 switch $1"
}

# Create a new tagged release based on the current state of the repositories.
cut_tagged_release() {
    # $1 = name of the tag.
    # $2 = build to cut from.
    # The build we are cutting from whould have already been pinned to the tag.
    release_exists "$1" && die "Cannot cut tagged release $1, it already exists!"
    build_exists "$2" || die "Cannot cut tagged release, basis build $2 does not exist!"
    local bc ref f
    mkdir -p "$CROWBAR_DIR/releases/$1/master"
    for bc in $(barclamps_in_build "$2"); do
        ref=$(barclamp_branch_for_build "$2" "$bc")
        [[ $ref && $ref != empty-branch ]] || continue
        # We assume that the barclamp tags have already been created.
        echo "$1" > "$CROWBAR_DIR/releases/$1/master/barclamp-$bc"
    done
    for f in "$CROWBAR_DIR/releases/$2/"*; do
        f=${f##*/$2/}
        [[ $f = barclamp-* ]] && continue
        [[ $f = . || $f = .. || $f = parent ]] && continue
        cp -a "$CROWBAR_DIR/releases/$2/$f" \
            "$CROWBAR_DIR/releases/$1/master"
    done
    if git_managed_cache; then
        local old_rel_br=$(release_branch "${2%/*}")
        local rel_br="$(release_branch "$1")"
        in_cache branch_exists "$old_rel_br" && \
            in_cache git branch "$rel_br" "$old_rel_br"
    fi
    in_repo git add "releases/$1"
    in_repo git commit -m "Cut tagged release $1"
}

# Erase a release.  Complains if it is not merged into its parent release.
erase_release() {
    # $1 = release refix
    local bc build whine=false current_br parent_br template_br
    local -A branches
    [[ $2 ]] && die "erase-feature only takes one argument"
    [[ $1 = development ]] && die "Cannot erase the development release."
    release_exists "$1" || die "$1 is not a release we can erase!"
    parent=$(find_best_parent "$1")
    template_br=$(release_branch "$1")
    for build in $(builds_in_release "$1"); do
        if ! [[ -d $CROWBAR_DIR/releases/$parent/$build ]]; then
            debug "$build does not exist in $parent release."
            whine=true
            continue
        fi
        for bc in $(barclamps_from_build "$1/$build"); do
            if ! __barclamp_exists_in_build "$parent/$build/$bc"; then
                debug "$barclamp does not exist in $parent/$build"
                whine=true
                continue
            fi
            current_br=$(barclamp_branch_for_build "$1/$build" "$bc")
            [[ $current_br && $current_br != empty-branch ]] || continue
            parent_br=$(barclamp_branch_for_build "$parent/$build" "$bc")
            if [[ ! $parent_br || $parent_br = empty-branch ]]; then
                debug "$1/$build/$bc: branch ${release_refs[$bc]} is unique to $1."
                whine=true
            fi
            if [[ $current_br != $template_br ]]; then
                debug "Barclamp $bc is on $current_br, which is not the expected branch name." \
                    "We expected it to be on $template_br"
            fi
            if ! in_barclamp "$bc" branches_synced . "$parent_br" "$current_br"; then
                debug "barclamp $bc: $current_br is not merged into $parent_br"
                whine=true
            fi
        done
    done
    if [[ $whine = true ]]; then
        printf "$1 is not merged into $parent.  Really erase? (y/n): " >&2
        read -n 1
        [[ $REPLY != 'y' ]] && exit
    fi
    debug "Erasing branches for release $1"
    for bc in $(barclamps_in_release "$1"); do
        for current_br in $(barclamp_branches_for_release "$1" "$bc"); do
            in_barclamp "$bc" git branch -D "${current_br}" &>/dev/null
        done
    done
    debug "Erasing metadata for release $1"
    {   in_repo git rm -rf "releases/$1"
        in_repo rm -rf "releases/$1"
        in_repo git commit -m "Erasing release $1"
    } &>/dev/null
}

# Given a release, show any redundant barclamp declarations.
# This is intended to help manually clean up release metadata.
show_duplicate_barclamps() {
    local release=${1:-$(current_release)} build parent bc
    local to_remove=()
    for build in $(builds_in_release "$release"); do
        build="$release/$build"
        parent=$(parent_build "$build")
        [[ $parent ]] || continue
        local -A barclamps
        for bc in $(barclamps_in_build "$parent"); do
            barclamps["$bc"]=parent
        done
        for bc in $(barclamps_from_build "$build"); do
            if [[ ${barclamps[$bc]} ]] && diff -q \
                "$CROWBAR_DIR/releases/$build/barclamp-$bc" \
                "$CROWBAR_DIR/releases/$parent/barclamp-$bc" &>/dev/null; then
                to_remove+=("releases/$build/barclamp-$bc")
            fi
        done
    done
    if [[ $to_remove ]]; then
        debug "Release $release has the following redundant barclamp metadata:"
        echo "${to_remove[*]}"
    else
        debug "No redundant barclamps in release $release"
    fi
}
