#!/bin/bash
# Copyright 2011, Dell 
# 
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
# 
#  http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
# 
# Author: VictorLowther
#

# This script expects to be able to run certian commands as root.
# Either run it as a user who can sudo to root, or give the user
# you are running it as the following sudo rights:
# crowbar-tester ALL = NOPASSWD: /bin/mount, /bin/umount, /usr/sbin/debootstrap, /bin/cp, /usr/sbin/chroot

# When running this script for the first time, it will automatically create a
# cache directory and try to populate it with all the build dependencies.
# After that, if you need to pull in new dependencies, you will need to
# call the script with the --update-cache parameter.  If you are going to 
# develop on Crowbar, it is a good idea to put the build cache in its own git
# repository, and create a branching structure for the packages that mirrors
# the branching structure in the crowbar repository -- if you do that, then
# this build script can be smarter about what packages it should pull in
# whenever you invoke it to build an iso.


GEM_RE='([^0-9].*)-([0-9].*)'
[[ $DEBUG ]] && {
    set -x
    export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
}

readonly currdir="$PWD"
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin"


# Our general cleanup function.  It is called as a trap whenever the 
# build script exits, and it's job is to make sure we leave the local 
# system in the same state we cound it, modulo a few calories of wasted heat 
# and a shiny new .iso.
cleanup() {
    # Clean up any stray mounts we may have left behind. 
    # The paranoia with the grepping is to ensure that we do not 
    # inadvertently umount everything.
    GREPOPTS=()
    [[ $CACHE_DIR ]] && GREPOPTS=(-e "$CACHE_DIR")
    [[ $IMAGE_DIR && $CACHE_DIR =~ $IMAGE_DIR ]] && GREPOPTS+=(-e "$IMAGE_DIR")
    [[ $BUILD_DIR && $CACHE_DIR =~ $BUILD_DIR ]] && GREPOPTS+=(-e "$BUILD_DIR")
    [[ $CHROOT && $CACHE_DIR =~ $CHROOT ]] && GREPOPTS+=(-e "$CHROOT")
    if [[ $GREPOPTS ]]; then
	while read dev fs type opts rest; do
	    sudo umount -d -l "$fs"
	done < <(tac /proc/self/mounts |grep "${GREPOPTS[@]}")
    fi
    # If the build process spawned a copy of webrick, make sure it is dead.
    [[ $webrick_pid && -d /proc/$webrick_pid ]] && kill -9 $webrick_pid
    # clean up after outselves from merging branches, if needed.
    cd "$CROWBAR_DIR"
    if [[ $THROWAWAY_BRANCH ]]; then
	# Check out the branch we started the build process, and then 
	# nuke whatever throwaway branch we may have created.
	git checkout -f "${CURRENT_BRANCH##*/}" &>/dev/null
	git branch -D "$THROWAWAY_BRANCH" &>/dev/null
    fi
    # If we saved unadded changes, resurrect them.
    [[ $THROWAWAY_STASH ]] && git stash apply "$THROWAWAY_STASH" &>/dev/null
    # Do the same thing as above, but for the build cache instead.
    cd "$CACHE_DIR"
    if [[ $CACHE_THROWAWAY_BRANCH ]]; then
	git checkout -f "$CURRENT_CACHE_BRANCH" &>/dev/null
	git branch -D "$CACHE_THROWAWAY_BRANCH" &>/dev/null
    fi
    [[ $CACHE_THROWAWAY_STASH ]] && git stash apply "$CACHE_THROWAWAY_STASH"
    for d in "$IMAGE_DIR" "$BUILD_DIR" "$CHROOT"; do
	[[ -d $d ]] && sudo rm -rf -- "$d"
    done
}

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
    local pkgname='' pkg=''
    while read pkg; do
	[[ -f $pkg ]] && is_pkg "$pkg" || continue
	pkgname="$(pkg_name "$pkg")"
	CD_POOL["$pkgname"]="${pkg}"
    done < <(find "$(find_cd_pool)" -type f)
}

# Make a chroot environment for package-fetching purposes. 
make_chroot() {
    [[ -f $CHROOT/etc/resolv.conf ]] && return 0
    local bc repo
    debug "Making package-fetching chroot"
    mkdir -p "$CHROOT/$CHROOT_PKGDIR"
    mkdir -p "$CHROOT/$CHROOT_GEMDIR"
    __make_chroot

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
    chroot_update
}


stage_pkgs() {
    # $1 = cache to copy from.
    # $2 = location to copy to
    local pkg pkgname pkg_t
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
		rm -f "$pkg_t"
		cp "$pkg" "$2"
		STAGED_POOL["$pkgname"]="$2/${pkg##*/}"
	    fi
	else
	    # We have not seen this package before.  Copy it.
	    cp "$pkg" "$2"
	    STAGED_POOL["$pkgname"]="$2/${pkg##*/}"
	fi
    done < <(find "$1" -type f)
}

# Update the package cache for a barclamp.
update_barclamp_pkg_cache() {
    # $1 = barclamp we are working with
    local bc_cache="$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs" pkg dest bc
    local -A pkgs
    ( cd "$CHROOT/$CHROOT_PKGDIR" && sudo rm -rf * )
    for bc in $(all_deps "$1"); do
	[[ -d "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/." ]] || continue
	sudo cp -a "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/." \
	    "$CHROOT/$CHROOT_PKGDIR"
    done
    # Remember what packages we already have.
    while read pkg; do
	is_pkg "$pkg" || continue
	pkgs["$pkg"]="true"
    done < <(find "$CHROOT/$CHROOT_PKGDIR" -type f)
    chroot_fetch ${BC_PKGS["$1"]} ${BC_BUILD_PKGS["$1"]} || \
	die "Could not fetch packages required by barclamp $1"
    while read pkg; do
	is_pkg "$pkg" || continue
	[[ ${pkgs["$pkg"]} = true ]] && continue
	cp "$pkg" "$bc_cache"
    done < <(find "$CHROOT/$CHROOT_PKGDIR" -type f)
}

# Update the gem cache for a barclamp
update_barclamp_gem_cache() {
    local -A gems
    local gemname gemver gemopts bc gem
    local bc_cache="$CACHE_DIR/barclamps/$1/gems"

    # Wipe out the caches.
    ( cd "$CHROOT/$CHROOT_GEMDIR" && sudo rm -rf * )
    # if we have deb or gem caches, copy them back in.
    # Make sure we copy the caches for all our dependent barclamps.
    for bc in $(all_deps "$1"); do
	[[ -d "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/." ]] && \
	    sudo cp -a "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs/." \
	    "$CHROOT/$CHROOT_PKGDIR"
	[[ -d "$CACHE_DIR/barclamps/$bc/gems/." ]] && \
	    sudo cp -a "$CACHE_DIR/barclamps/$bc/gems/." \
	    "$CHROOT/$CHROOT_GEMDIR"
    done
    
    while read gem; do
	[[ $gem = *.gem ]] || continue
	gems["$gem"]="true"
    done < <(find "$CHROOT/$CHROOT_GEMDIR" -type f)
        
    # install any build dependencies we need.
    chroot_install ${BC_BUILD_PKGS["$1"]}
    # Grab the gems needed for this barclamp.
    for gem in ${BC_GEMS["$1"]}; do
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

    # Save our updated gems and pkgs in the cache for later.
     while read gem; do
	[[ $gem = *.gem ]] || continue
	[[ ${gems["$gem"]} = "true" ]] && continue
	cp "$gem" "$bc_cache"
    done < <(find "$CHROOT/$CHROOT_GEMDIR" -type f)
}

# Fetch any raw packages we do not already have.
update_barclamp_raw_pkg_cache() {
    local pkg bc_cache="$CACHE_DIR/barclamps/$1/$OS_TOKEN/pkgs"
    # Fetch any raw_pkgs we were asked to.
    for pkg in ${BC_RAW_PKGS["$1"]}; do
	[[ -f $bc_cache/${pkg##*/} ]] && continue
	curl -s -S -o "$bc_cache/${pkg##*/}" "$pkg"
    done
}

# Fetch any bare files that we do not already have.
update_barclamp_file_cache() {
    local dest pkg bc_cache="$CACHE_DIR/barclamps/$1/files"
    # Fetch any extra_pkgs we need.
    while read pkg; do
	dest=${pkg#* }
	[[ $dest = $pkg ]] && dest=''
	pkg=${pkg%% *}
	[[ -f $bc_cache/files/$dest/${pkg##*/} ]] && continue
	mkdir -p "$bc_cache/$dest"
	curl -s -S -o "$bc_cache/$dest/${pkg##*/}" "$pkg"
    done < <(write_lines "${BC_EXTRA_FILES[$1]}")
}

# Check to see if the barclamp package cache needs update.
barclamp_pkg_cache_needs_update() {
    local pkg pkgname arch bc 
    local -A pkgs

    # First, check to see if we have all the packages we need.
    for bc in $(all_deps "$1"); do
	local bc_cache="$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs"
	mkdir -p "$bc_cache"
	while read pkg; do
	    is_pkg "$pkg" || continue
	    pkgname="$(pkg_name "$pkg")"
	    #debug "$pkgname is cached"
	    pkgs["$pkgname"]="$pkg"
	done < <(find "$bc_cache" -type f)
    done 
    for pkg in ${BC_PKGS["$1"]} ${BC_BUILD_PKGS["$1"]}; do
	[[ $pkg ]] || continue
	for arch in "${PKG_ALLOWED_ARCHES[@]}"; do
	    [[ ${pkgs["$pkg-$arch"]} || ${CD_POOL["$pkg-$arch"]} ]] \
		&& continue 2
	    #debug "Could not find $pkg-$arch"
	done
	return 0
    done
    return 1
}

# Check to see if the barclamp gem cache needs an update.
barclamp_gem_cache_needs_update() {
    local pkg pkgname 
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
    for pkg in ${BC_RAW_PKGS["$1"]}; do
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

# Arrange for cleanup to be called at the most common exit points.
trap cleanup 0 INT QUIT TERM

# Source our config file if we have one
[[ -f $HOME/.build-crowbar.conf ]] && \
    . "$HOME/.build-crowbar.conf"

# Look for a local one.
[[ -f build-crowbar.conf ]] && \
    . "build-crowbar.conf"

# Set up our proxies if we were asked to.
if [[ $USE_PROXY = "1" && $PROXY_HOST ]]; then
    proxy_str="http://"
    if [[ $PROXY_PASSWORD && $PROXY_USER ]]; then
	proxy_str+="$PROXY_USER:$PROXY_PASSWORD@"
    elif [[ $PROXY_USER ]]; then
	proxy_str+="$PROXY_USER@"
    fi
    proxy_str+="$PROXY_HOST"
    [[ $PROXY_PORT ]] && proxy_str+=":$PROXY_PORT"
    [[ $no_proxy ]] || no_proxy="localhost,localhost.localdomain,127.0.0.0/8,$PROXY_HOST" 
    [[ $http_proxy ]] || http_proxy="$proxy_str/" 
    [[ $https_proxy ]] || https_proxy="$http_proxy"
    export no_proxy http_proxy https_proxy
else
    unset no_proxy http_proxy https_proxy
fi

# Next, some configuration variables that can be used to tune how the 
# build process works.

# Barclamps to include.  By default, start with jsut crowbar and let
# the dependency machinery and the command line pull in the rest.
# Note that BARCLAMPS is an array, not a string!
[[ $BARCLAMPS ]] || BARCLAMPS=()

# Location for caches that should not be erased between runs
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.crowbar-build-cache"

# Location to store .iso images that we use in the build process.
# These are usually OS install DVDs that we will stage Crowbar on to.
[[ $ISO_LIBRARY ]] || ISO_LIBRARY="$CACHE_DIR/iso"

# This is the location that we will save the generated .iso to.
[[ $ISO_DEST ]] || ISO_DEST="$PWD"

# Directory that holds our Sledgehammer PXE tree.
[[ $SLEDGEHAMMER_PXE_DIR ]] || SLEDGEHAMMER_PXE_DIR="$CACHE_DIR/tftpboot"

# Location of the Crowbar checkout we are building from.
[[ $CROWBAR_DIR ]] ||CROWBAR_DIR="${0%/*}"

# Location of the Sledgehammer source tree.  Only used if we cannot 
# find Sledgehammer in $SLEDGEHAMMER_PXE_DIR above. 
[[ $SLEDGEHAMMER_DIR ]] || SLEDGEHAMMER_DIR="${CROWBAR_DIR}/../sledgehammer"

# Command to run to clean out the tree before starting the build.
# By default we want to be relatively pristine.
[[ $VCS_CLEAN_CMD ]] || VCS_CLEAN_CMD='git clean -f -x -d'

# Arrays holding the additional pkgs and gems populate Crowbar with.
REPOS=()

declare -A CD_POOL STAGED_POOL

# Some helper functions

# Print a message to stderr and exit.  cleanup will be called.
die() { echo "$(date '+%F %T %z'): $*" >&2; exit 1; }

# Print a message to stderr and keep going.
debug() { echo "$(date '+%F %T %z'): $*" >&2; }

# Clean up any cruft that we might have left behind from the last run.
clean_dirs() {
    local d=''
    for d in "$@"; do
	(   mkdir -p "$d"
	    cd "$d"
	    chmod -R u+w .
	    rm -rf * )
    done
}

# Verify that the passed name is really a branch in the git repo.
branch_exists() { git show-ref --quiet --verify --heads -- "refs/heads/$1"; }

# Run a git command in the crowbar repo.
in_repo() ( cd "$CROWBAR_DIR"; "$@")

# Get the head revision of a git repository.
get_rev() (
    cd "$1"
    if [[ -d .git ]]; then
	git rev-parse HEAD
    else
	echo "Not a Git Repository"
    fi
)

# Run a git command in the build cache, assuming it is a git repository. 
in_cache() (
    [[ $CURRENT_CACHE_BRANCH ]] || return 
    cd "$CACHE_DIR"
    "$@"
)

# Check to see if something is a barclamp.
is_barclamp() { [[ -f $CROWBAR_DIR/barclamps/$1/crowbar.yml ]]; }

# Get the OS we were asked to stage Crowbar on to.  Assume it is Ubuntu 10.10
# unless we specify otherwise.
OS_TO_STAGE="${1-ubuntu-10.10}"
shift

# Make sure that we actually know how to build the ISO we were asked to 
# build.  If we do not, print a helpful error message.
if ! [[ $OS_TO_STAGE && -d $CROWBAR_DIR/$OS_TO_STAGE-extra && \
    -f $CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh ]]; then
    cat <<EOF
You must pass the name of the operating system you want to stage Crowbar
on to.  Valid choices are:
EOF
    cd "$CROWBAR_DIR"
    for d in *-extra; do
	[[ -d $d && -f $d/build_lib.sh ]] || continue
	echo "    ${d%-extra}"
    done
    exit 1
fi

# Source OS specific build knowledge.  This includes:
# Parameters that build_crowbar.sh needs to know:
# OS = the distribution we are staging on to, such as redhat or ubuntu.
# OS_VERSION = the version of the distribution we are staging on to.
#              For redhat, it would be somethibng like 5.6
# OS_TOKEN = Defaults to "$OS-$OS_VERSION"
# ISO = the name of the install ISO image we are going to stage Crowbar on to.
# PKG_TYPE = The package type we should look for in crowbar.yml for OS
#            specific packages.
# PKG_ALLOWED_ARCHES = The allowed package arches we will look to stage.
#                      These are usually whatever the OS equivalent of 
#                      amd64 and all are.
# CHROOT_GEMDIR = The location that the OS keeps its gem cache in.
# CHROOT_PKGDIR = The location that the OS keeps its package cache in.
# Functions that build_crowbar needs to call:
# find_cd_pool(): This function should echo the current location of the
#   package pool on $ISO when it is mounted on $IMAGE_DIR.
# fetch_os_iso(): This function should try to fetch the OS iso from
#   a well-known location on the Internet, and die if it cannot.
# chroot_update(): This function should ask the chroot to update its
#   package metadata.
# chroot_install(): This function should try to install the packages passed
#   to it as args.
# chroot_fetch(): This function should ask to download (but not install)
#   the packages passed to it as arguments and all their dependencies.
# add_repos(): This function should try to add repositories passed to it
#   as args to the chroot environment.
# is_pkg(): This function should check to see if the string passed to it
#   is a valid package filename.  The file need not actually exist.
# pkg_name(): This function should extract the package name and arch as the
#   package management system will see them from the file passed as an argument,
#   and echo that information in $name-$arch format.
# __make_chroot(): This function should handle all of the OS-specific actions
#   needed to set up the package-fetching chroot environment.
# pkg_cmp(): This function should check to see that both of the files passed to
#   it refer to the same package name.  If it does, this function should return
#   0 if the first package is a higher revision than the second one, and 1 if 
#   the first package is the same or lower revision.  It should die if the
#   package names are not the same.
# final_build_fixups(): This function should take wahtever steps are needed
#   to make the default OS install process also ensure that the Crowbar bits 
#   are properly staged and to completly automate the admin node install 
#   process, either as an install from CD or an install via PXE.  This 
#   usually entails modifying initrd files, adding kickstarts/install seeds,
#   modifying boot config files, and so on.
. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh"

{
    # Make sure only one instance of the ISO build runs at a time.
    # Otherwise you can easily end up with a corrupted image.
    debug "Acquiring the build lock."
    flock 65
    # Figure out what our current branch is, in case we need to merge 
    # other branches in to the iso to create our build.  
    CURRENT_BRANCH="$(in_repo git symbolic-ref HEAD)" || \
	die "Not on a branch we can build from!"
    CURRENT_BRANCH=${CURRENT_BRANCH##*/}
    [[ $CURRENT_BRANCH ]] || die "Not on a branch we can merge from!"
    
    # Check and see if our local build repository is a git repo. If it is,
    # we may need to do the same sort of merging in it that we might do in the 
    # Crowbar repository.
    if [[ -d $CACHE_DIR/.git ]]; then
	for br in "$CURRENT_BRANCH" master ''; do
	    [[ $br ]] || die "Cannot find $CURRENT_BRANCH or master in $CACHE_DIR"
	    (cd "$CACHE_DIR"; branch_exists "$br") || continue
	    CURRENT_CACHE_BRANCH="$br"
	    break
	done
	# If there are packages that have not been comitted, save them
	# in a stash before continuing.  We do this on the assumption that
	# these packages were added manually for testing purposes, or were
	# added in an earlier update-cache operation, but that the user has
	# not gotten around to comitting yet.
	if [[ ! $(in_cache git status) =~ working\ directory\ clean ]]; then
	    CACHE_THROWAWAY_STASH=$(in_cache git stash create)
	    in_cache git checkout -f .
	fi
    fi

    # Parse our options.  
    while [[ $1 ]]; do
	case $1 in
	    # Merge a list of branches into a throwaway branch with the 
	    # current branch as a baseline before starting the rest of the 
	    # build process.  This makes it easier to spin up iso images 
	    # with local changes without having to manually merge those 
	    # changes in with any other branches of interest first.  
	    # This code takes heavy advantage of the lightweight nature of 
	    # git branches and takes care to leave uncomitted changes in place.
	    -m|--merge)
		shift
		# Loop through the rest of the arguments, as long as they
		# do not start with a -.
		while [[ $1 && ! ( $1 = -* ) ]]; do
		    # Check to make sure that this argument refers to a branch
		    # in the crowbar git tree.  Die if it does not.
		    in_repo branch_exists "$1" || die "$1 is not a git branch!"
		    # If we have not already created a throwaway branch to
		    # merge these branches into, do so now. If we have 
		    # uncomitted changes that need to be stashed, do so here.
		    if [[ ! $THROWAWAY_BRANCH ]]; then
			THROWAWAY_BRANCH="build-throwaway-$$-$RANDOM"
			REPO_PWD="$PWD"
			if [[ ! $(in_repo git status) =~ working\ directory\ clean ]]; then
			    THROWAWAY_STASH=$(in_repo git stash create)
			    in_repo git checkout -f .
			fi
			in_repo git checkout -b "$THROWAWAY_BRANCH"
		    fi
		    # Merge the requested branch into the throwaway branch.
		    # Die if the merge failed -- there must have been a
		    # conflict, and the user needs to fix it up.
		    in_repo git merge "$1" || \
			die "Merge of $1 failed, fix things up and continue"
		    # If there is n identically named branch in the build cache,
		    # merge it into a throwaway branch of the build cache
		    # along with the current branch in the build cache.
		    # This makes it easier to include and manage packages that
		    # are branch-specific, but that do not need to be included
		    # in every build.
		    if in_cache branch_exists "$1"; then
			if [[ ! $CACHE_THROWAWAY_BRANCH ]]; then
			    CACHE_THROWAWAY_BRANCH=${THROWAWAY_BRANCH/build/cache}
			    in_cache git checkout -b "$CACHE_THROWAWAY_BRANCH"
			fi
			in_cache git merge "$1" || \
			    die "Could not merge build cache branch $1"
		    fi
		    shift
		done
		;;
	    # Force an update of the cache
	    update-cache|--update-cache) shift; need_update=true;;
	    # Pull in additional barclamps.
	    --barclamps)
		shift
		while [[ $1 && $1 != -* ]]; do
		    BARCLAMPS+=("$1")
		    shift
		done;;
	    --test)
		NEED_TEST=true
		test_params=(run-test)
		shift
		while [[ $1 && $1 != -* ]]; do
		    test_params+=("$1")
		    shift
		done;;
	    *) 	die "Unknown command line parameter $1";;
	esac
    done

    # If we stached changes to the crowbar repo, apply them now.
    [[ $THROWAWAY_STASH ]] && in_repo git stash apply "$THROWAWAY_STASH"
    # Ditto for the build cache.
    [[ $CACHE_THROWAWAY_STASH ]] && \
	in_cache git stash apply "$CACHE_THROWAWAY_STASH" 

    # Finalize where we expect to find our caches and out chroot.
    # If they were set in one of the conf files, don't touch them.

    # The directory we perform a minimal install into if we need
    # to refresh our gem or pkg caches
    [[ $CHROOT ]] || CHROOT="$CACHE_DIR/$OS_TOKEN/chroot"

    # Make sure that the $OS_TOKEN directory exist.
    mkdir -p "$CACHE_DIR/$OS_TOKEN"
    
    # The directory we will stage the build into.
    [[ $BUILD_DIR ]] || \
	BUILD_DIR="$(mktemp -d "$CACHE_DIR/$OS_TOKEN/build-XXXXX")"
    # The directory that we will mount the OS .ISO on .
    [[ $IMAGE_DIR ]] || \
	IMAGE_DIR="$CACHE_DIR/$OS_TOKEN/image-${BUILD_DIR##*-}"

    # Directory where we will look for our package lists
    [[ $PACKAGE_LISTS ]] || PACKAGE_LISTS="$BUILD_DIR/extra/packages"

    # Hashes to hold our "interesting" information.
    # Key = barclamp name
    # Value = whatever interesting thing we are looking for.
    declare -A BC_DEPS BC_GROUPS BC_PKGS BC_EXTRA_FILES BC_OS_DEPS BC_GEMS
    declare -A BC_REPOS BC_PPAS BC_RAW_PKGS BC_BUILD_PKGS BC_QUERY_STRINGS
    
    # Query strings to pull info we are interested out of crowbar.yml
    BC_QUERY_STRINGS["deps"]="barclamp requires"
    BC_QUERY_STRINGS["groups"]="barclamp member"
    BC_QUERY_STRINGS["pkgs"]="$PKG_TYPE pkgs"
    BC_QUERY_STRINGS["extra_files"]="extra_files"
    BC_QUERY_STRINGS["os_support"]="barclamp os_support"
    BC_QUERY_STRINGS["gems"]="gems pkgs"
    BC_QUERY_STRINGS["repos"]="$PKG_TYPE repos"
    BC_QUERY_STRINGS["ppas"]="$PKG_TYPE ppas"
    BC_QUERY_STRINGS["build_pkgs"]="$PKG_TYPE build_pkgs"
    BC_QUERY_STRINGS["raw_pkgs"]="$PKG_TYPE raw_pkgs"
    
    # Pull in interesting information from all our barclamps
    for bc in $CROWBAR_DIR/barclamps/*; do
	[[ -d $bc ]] || continue
	bc=${bc##*/}
	debug "Reading metadata for $bc barclamp."
	is_barclamp "$bc" || die "$bc is not a barclamp!"
	yml_file="$CROWBAR_DIR/barclamps/$bc/crowbar.yml"
	for query in "${!BC_QUERY_STRINGS[@]}"; do
	    while read line; do
		[[ $line = nil ]] && continue
		case $query in
		    deps) is_in "$line "${BC_DEPS["$bc"]} || \
			BC_DEPS["$bc"]+="$line ";;
		    groups) is_in "$line" ${BC_GROUPS["$bc"]} || 
			BC_GROUPS["$line"]+="$bc ";;
		    pkgs) is_in "$line" ${BC_PKGS["$bc"]} || \
			BC_PKGS["$bc"]+="$line ";;
		    extra_files) BC_EXTRA_FILES["$bc"]+="$line\n";;
		    os_support) BC_OS_SUPPORT["$bc"]+="$line ";;
		    gems) BC_GEMS["$bc"]+="$line ";;
		    repos) BC_REPOS["$bc"]+="$line\n";;
		    ppas) [[ $PKG_TYPE = debs ]] || \
			die "Cannot declare a PPA for $PKG_TYPE!"
			BC_REPOS["$bc"]+="ppa $line\n";;
		    build_pkgs) BC_BUILD_PKGS["$bc"]+="$line ";;
		    raw_pkgs) BC_RAW_PKGS["$bc"]+="$line ";;
		    *) die "Cannot handle query for $query."
		esac
	    done < <("$CROWBAR_DIR/parse_yml.rb" \
		"$yml_file" \
		${BC_QUERY_STRINGS["$query"]} 2>/dev/null)
	done
    done

    debug "Analyzing barclamp group membership"
    # If any barclamps need group expansion, do it.
    for bc in "${!BC_DEPS[@]}"; do
	newdeps=''
	for dep in ${BC_DEPS["$bc"]}; do
	    if [[ $dep = @* ]]; then
		[[ ${BC_GROUPS["${dep#@}"]} ]] || \
		    die "$bc depends on group ${dep#@}, but that group does not exist!"
		for d in ${BC_GROUPS["${dep#@}"]}; do
		    is_barclamp "$d" || \
			die "$bc depends on barclamp $d from group ${dep#@}, but $d does not exist!"
		    newdeps+="$d "
		done
	    else
		is_barclamp "$dep" || \
		    die "$bc depends on barclamp $dep, but $dep is not a barclamp!"
		newdeps+="$dep "
	    fi
	done
	BC_DEPS["$bc"]="$newdeps"
    done

    # Proxy Variables
    [[ $USE_PROXY ]] || USE_PROXY=0
    [[ $PROXY_HOST ]] || PROXY_HOST=""
    [[ $PROXY_PORT ]] || PROXY_PORT=""
    [[ $PROXY_USER ]] || PROXY_USER=""
    [[ $PROXY_ESC_USER ]] || PROXY_ESC_USER=""
    [[ $PROXY_PASSWORD ]] || PROXY_PASSWORD=""

    # Version for ISO
    [[ $VERSION ]] || VERSION="$(cd "$CROWBAR_DIR"; git describe --long --tags)-dev"

    # Name of the built iso we will build
    [[ $BUILT_ISO ]] || BUILT_ISO="crowbar-${VERSION}.iso"

    # If we were not passed a list of barclamps to include,
    # pull in all of the ones declared as submodules.
    [[ $BARCLAMPS ]] || BARCLAMPS=($(cd "$CROWBAR_DIR"
	    while read sha submod branch; do
		[[ $submod = barclamps/* ]] || continue
		[[ -f $submod/crowbar.yml ]] || \
		    echo "Cannot find crowbar.yml for $submod, exiting."
		echo "${submod##*/}"
	    done < <(git submodule status))
	)
    
    # Group-expand barclamps if needed, and unset groups after they are expanded.
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
	    for dep in ${BC_DEPS["$bc"]}; do
		is_in "$dep" "${new_barclamps[@]}" && continue
		new_barclamps+=("$dep")
	    done
	    is_in "$bc" "${new_barclamps[@]}" || new_barclamps+=("$bc")
	done
	[[ ${BARCLAMPS[*]} = ${new_barclamps[*]} ]] && break
	BARCLAMPS=("${new_barclamps[@]}")
    done

    # Make any directories we don't already have
    for d in "$ISO_LIBRARY" "$ISO_DEST" "$IMAGE_DIR" "$BUILD_DIR" \
	"$SLEDGEHAMMER_PXE_DIR" "$CHROOT"; do
	mkdir -p "$d"
    done
    
    debug "Checking for Sledgehammer."
    # Make sure Sledgehammer has already been built and pre-staged.
    if ! [[ -f $SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz || \
	-f $SLEDGEHAMMER_PXE_DIR/initrd0.img ]]; then
	echo "Slegehammer TFTP image missing!"
	echo "Please build Sledgehammer from $SLEDGEHAMMER_DIR before building Crowbar."
	exit 1
    fi  
  
    # Fetch the OS ISO if we need to.
    [[ -f $ISO_LIBRARY/$ISO ]] || fetch_os_iso

    # Start with a clean slate.
    clean_dirs "$IMAGE_DIR" "$BUILD_DIR"
    
    debug "Cleaning up any VCS cruft."
    # Clean up any cruft that the editor may have left behind.
    (cd "$CROWBAR_DIR"; $VCS_CLEAN_CMD)

    # Make additional directories we will need.
    for d in discovery extra; do
	mkdir -p "$BUILD_DIR/$d"
    done

    # Mount our ISO for the build process.
    debug "Mounting $ISO"
    sudo mount -t iso9660 -o loop "$ISO_LIBRARY/$ISO" "$IMAGE_DIR" || \
	die "Could not mount $ISO"
    debug "Indexing CD package pool."
    index_cd_pool
    
    # Copy over the Crowbar bits and their prerequisites
    cp -r "$CROWBAR_DIR/extra"/* "$BUILD_DIR/extra"
    cp -r "$CROWBAR_DIR/$OS_TOKEN-extra"/* "$BUILD_DIR/extra"
    cp -r "$CROWBAR_DIR/change-image"/* "$BUILD_DIR"
    mkdir -p "$BUILD_DIR/dell/barclamps"
    echo "build-timestamp: $(date '+%F %T %z')" > "$BUILD_DIR/build-info"
    echo "build-os: $OS_TOKEN" >>"$BUILD_DIR/build-info"
    echo "build-os-iso: $ISO" >>"$BUILD_DIR/build-info"
    echo "crowbar: $(get_rev "$CROWBAR_DIR")" >>"$BUILD_DIR/build-info"
    for bc in "${BARCLAMPS[@]}"; do
	is_barclamp "$bc" || die "Cannot find barclamp $bc!"
	debug "Staging $bc barclamp."
	for cache in pkg gem raw_pkg file; do
	    checker="barclamp_${cache}_cache_needs_update"
	    updater="update_barclamp_${cache}_cache"
	    [[ $(type $checker) = "$checker is a function"* ]] || \
		die "Asked to check $cache cache, but no checker function!"
	    [[ $(type $updater) = "$updater is a function"* ]] || \
		die "Might need to update $cache cache, but no updater!"
	    if $checker "$bc" || [[ $need_update = true ]]; then
		debug "Updating $cache cache for $bc"
		[[ $cache =~ ^(pkg|gem)$ ]] && make_chroot
		$updater "$bc"
	    fi
	done
	cp -r "$CROWBAR_DIR/barclamps/$bc" "$BUILD_DIR/dell/barclamps"
	mkdir -p "$BUILD_DIR/extra/pkgs/"
	stage_pkgs "$CACHE_DIR/barclamps/$bc/$OS_TOKEN/pkgs" \
	    "$BUILD_DIR/extra/pkgs"
	cp -r "$CACHE_DIR/barclamps/$bc/files" \
	    "$CACHE_DIR/barclamps/$bc/gems" \
	    "$BUILD_DIR/extra"
	echo "barclamps/$bc: $(get_rev "$CROWBAR_DIR/barclamps/$bc")" >> "$BUILD_DIR/build-info"
    done

    (cd "$BUILD_DIR"
	find extra/pkgs extra/gems extra/files -type f -print | \
	    sort >> "build-info")
    # Make sure we still provide the legacy ami location
    (cd "$BUILD_DIR"; ln -sf extra/files/ami)
    # Store off the version
    echo "$VERSION" >> "$BUILD_DIR/dell/Version"

    # Custom start-up in place
    if [ -f "$CROWBAR_DIR/crowbar.json" ] ; then
      mkdir -p "$BUILD_DIR/extra/config"
      cp "$CROWBAR_DIR/crowbar.json" "$BUILD_DIR/extra/config"
    fi
   
    final_build_fixups
 
    # Copy over the bits that Sledgehammer will look for.
    debug "Copying over Sledgehammer bits"
    # If we need to copy over a new Sledgehammer image, do so.
    if [[ $SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz -nt \
	$SLEDGEHAMMER_PXE_DIR/initrd0.img ]]; then
	(   cd $SLEDGEHAMMER_PXE_DIR
	    debug "Extracting new Sledgehammer TFTP boot image"
	    rm -rf .
	    cd ..
	    tar xzf "$SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz"
	    rm -f "$SLEDGEHAMMER_DIR/bin/sledgehammer-tftpboot.tar.gz"
	)
    fi
    cp -a "$SLEDGEHAMMER_PXE_DIR"/* "$BUILD_DIR/discovery"

    # Make our image
    debug "Creating new ISO"
    # Find files and directories that mkisofs will complain about.
    # Do just top-level overlapping directories for now.
    for d in $(cat <(cd "$BUILD_DIR"; find -maxdepth 1 -type d ) \
	           <(cd "$IMAGE_DIR"; find -maxdepth 1 -type d) | \
	           sort |uniq -d); do
	[[ $d = . ]] && continue
	d=${d#./}
	# Copy contents of the found directories into $BUILD_DIR, taking care
	# to not clobber existing files.
	mkdir -p "$BUILD_DIR/$d"
	chmod u+wr "$BUILD_DIR/$d"
	# We could also use cp -n, but rhel5 and centos5 do not understand it.
	rsync -rl --ignore-existing --inplace "$IMAGE_DIR/$d/." "$BUILD_DIR/$d/."
	chmod -R u+wr "$BUILD_DIR/$d"
	# Bind mount an empty directory on the $IMAGE_DIR instance.
	sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/$d"
    done
    mkdir -p "$BUILD_DIR/isolinux"
    chmod u+wr "$BUILD_DIR/isolinux"
    rsync -rl --ignore-existing --inplace \
	"$IMAGE_DIR/isolinux/." "$BUILD_DIR/isolinux/."
    chmod -R u+wr "$BUILD_DIR/isolinux"
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/isolinux"
    
    # Make a file list and a link list.
    ( cd $BUILD_DIR
      find . -type f | \
	  sort > crowbar_files.list
      find . -type l | \
	  xargs ls -ld | \
	  awk '{ print $8 " " $10 }' | \
	  sort > crowbar_links.list
    )
    ( cd $IMAGE_DIR
      find . -type f | \
	  sort >> $BUILD_DIR/crowbar_files.list
      find . -type l | \
	  xargs ls -ld | \
	  awk '{ print $8 " " $10 }' | \
	  sort >> $BUILD_DIR/crowbar_links.list
    )

    # Make an ISO
    (   cd "$BUILD_DIR"
	rm -f isolinux/boot.cat
	find -name '.svn' -type d -exec rm -rf '{}' ';' 2>/dev/null >/dev/null
	find . -type f -not -name isolinux.bin -not -name sha1sums -not -path '*/.git/*' | \
	    xargs sha1sum -b >sha1sums
	mkdir -p "$ISO_DEST"
	# Save the sha1sums and the build-info files along side the iso.
	cp sha1sums build-info "$ISO_DEST"
	mkisofs -r -V "${VERSION:0:30}" -cache-inodes -J -l -quiet \
	    -b isolinux/isolinux.bin -c isolinux/boot.cat \
	    -no-emul-boot --boot-load-size 4 -boot-info-table \
	    -o "$ISO_DEST/$BUILT_ISO" "$IMAGE_DIR" "$BUILD_DIR" ) || \
	    die "There was a problem building our ISO."
    if [[ $NEED_TEST = true ]]; then
	[[ -L $HOME/test_framework ]] && rm -f "$HOME/test_framework"
	[[ -d $HOME/test_framework ]] && rm -rf "$HOME/test_framework"
	if [[ $CROWBAR_DIR = /* ]]; then
	    ln -sf "$CROWBAR_DIR/test_framework" "$HOME/test_framework"
	else
	    ln -sf "$currdir/$CROWBAR_DIR/test_framework" "$HOME/test_framework"
	fi
	(   unset no_proxy http_proxy https_proxy
	    "$HOME/test_framework/test_crowbar.sh" "${test_params[@]}" \
		use-iso "$ISO_DEST/$BUILT_ISO" 
	) || \
	    die "$(date '+%F %T %z'): Smoketest of $ISO_DEST/$BUILT_ISO failed."
    fi
    echo "$(date '+%F %T %z'): Finshed. Image at $ISO_DEST/$BUILT_ISO"
} 65> /tmp/.build_crowbar.lock
