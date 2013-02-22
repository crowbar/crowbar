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

# We always use the C language and locale
export LANG="C"
export LC_ALL="C"

GEM_RE='([^0-9].*)-([0-9].*)'

readonly currdir="$PWD"
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin"

# Source our config file if we have one
[[ -f $HOME/.build-crowbar.conf ]] && \
    . "$HOME/.build-crowbar.conf"

# Look for a local one.
[[ -f build-crowbar.conf ]] && \
    . "build-crowbar.conf"

# Always run in verbose mode for now.
VERBOSE=true

# Barclamps to include.  By default, start with jsut crowbar and let
# the dependency machinery and the command line pull in the rest.
# Note that BARCLAMPS is an array, not a string!
[[ $BARCLAMPS ]] || BARCLAMPS=()

[[ $ALLOW_CACHE_UPDATE ]] || ALLOW_CACHE_UPDATE=true

# Location to store .iso images that we use in the build process.
# These are usually OS install DVDs that we will stage Crowbar on to.
[[ $ISO_LIBRARY ]] || ISO_LIBRARY="$CACHE_DIR/iso"

# This is the location that we will save the generated .iso to.
[[ $ISO_DEST ]] || ISO_DEST="$PWD"

# Directory that holds our Sledgehammer PXE tree.
[[ $SLEDGEHAMMER_PXE_DIR ]] || SLEDGEHAMMER_PXE_DIR="$CACHE_DIR/tftpboot"

# Location of the Crowbar checkout we are building from.
[[ $CROWBAR_DIR ]] || CROWBAR_DIR="${0%/*}"
[[ $CROWBAR_DIR = /* ]] || CROWBAR_DIR="$currdir/$CROWBAR_DIR"
[[ -f $CROWBAR_DIR/build_crowbar.sh && -d $CROWBAR_DIR/.git ]] || \
    die "$CROWBAR_DIR is not a git checkout of Crowbar!"
export CROWBAR_DIR

# Command to run to clean out the tree before starting the build.
# By default we want to be relatively pristine.
[[ $VCS_CLEAN_CMD ]] || VCS_CLEAN_CMD='git clean -f -d'

# Arrays holding the additional pkgs and gems populate Crowbar with.
REPOS=()

declare -A CD_POOL STAGED_POOL INSTALLED_PKGS FORCE_BARCLAMP_UPDATE

# Get the OS we were asked to stage Crowbar on to.  Assume it is Ubuntu 10.10
# unless we specify otherwise.
OS_TO_STAGE="${1-ubuntu-10.10}"
shift

BC_PKG_TYPE="tar"

unset CROWBAR_BUILD_PID
# Source our common build functions
. "$CROWBAR_DIR/build_lib.sh" || exit 1
. "$CROWBAR_DIR/test_lib.sh" || exit 1

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
# add_offline_repos(): This function should take whatever steps are needed
#   to ensure that the OS packages in the crowbar build cache are installable
#   from the chroot environment.
. "$CROWBAR_DIR/$OS_TO_STAGE-extra/build_lib.sh"

# Build OS dependent query strings
# These have to be created after we know what OS we are building on.
BC_QUERY_STRINGS["pkgs"]="$PKG_TYPE pkgs"
BC_QUERY_STRINGS["src_pkgs"]="$PKG_TYPE src_pkgs"
BC_QUERY_STRINGS["repos"]="$PKG_TYPE repos"
BC_QUERY_STRINGS["ppas"]="$PKG_TYPE ppas"
BC_QUERY_STRINGS["build_pkgs"]="$PKG_TYPE build_pkgs"
BC_QUERY_STRINGS["raw_pkgs"]="$PKG_TYPE raw_pkgs"
BC_QUERY_STRINGS["pkg_sources"]="$PKG_TYPE pkg_sources"
BC_QUERY_STRINGS["os_pkgs"]="$PKG_TYPE $OS_TOKEN pkgs"
BC_QUERY_STRINGS["os_repos"]="$PKG_TYPE $OS_TOKEN repos"
BC_QUERY_STRINGS["os_ppas"]="$PKG_TYPE $OS_TOKEN ppas"
BC_QUERY_STRINGS["os_build_pkgs"]="$PKG_TYPE $OS_TOKEN build_pkgs"
BC_QUERY_STRINGS["os_raw_pkgs"]="$PKG_TYPE $OS_TOKEN raw_pkgs"
BC_QUERY_STRINGS["os_pkg_sources"]="$PKG_TYPE $OS_TOKEN pkg_sources"
BC_QUERY_STRINGS["os_build_cmd"]="$PKG_TYPE $OS_TOKEN build_cmd"

# Check to make sure our required commands are installed.
for cmd in sudo chroot mkisofs ruby curl; do
    which "$cmd" &>/dev/null || \
	die 1 "Please install $cmd before trying to build Crowbar."
done

flat_checkout || die "Checkout must be flat before starting build!"

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
	    BRANCHES_TO_MERGE=()
	    [[ $(in_repo git status) =~ working\ directory\ clean ]] || \
		die "Working tree must be clean before merging branches for build!"
	    while [[ $1 && ! ( $1 = -* ) ]]; do
		# Check to make sure that this argument refers to a branch
		# in the crowbar git tree.  Die if it does not.
		in_repo branch_exists "$1" || die "$1 is not a git branch!"
		BRANCHES_TO_MERGE+=("$1")
		shift
	    done
	    ;;
	# Force an update of the cache
	update-cache|--update-cache) shift;
	    need_update=true
	    ALLOW_CACHE_UPDATE=true
	    ALLOW_CACHE_METADATA_UPDATE=true
	    while [[ $1 && $1 != -* ]]; do
		is_barclamp "$1" || \
		    die "Cannot update non-barclamp $1."
		FORCE_BARCLAMP_UPDATE["$1"]=true
		unset need_update || : &>/dev/null
		shift
	    done;;
        --barclamps)
            shift
            while [[ $1 && $1 != -* ]]; do
                BARCLAMPS+=("$1")
                shift
            done;;
	--test)
	    # Run tests on the newly-created repository.
	    # The test framework does the heavy lifting.
	    NEED_TEST=true
	    test_params=()
	    shift
	    while [[ $1 && $1 != -* ]]; do
		test_params+=("$1")
		shift
	    done;;
	--shrink)
	    # Ask that the generated ISO be shrunk down to the mininim
	    # needed to deploy Crowbar.
	    type shrink_iso >&/dev/null || \
		die "The build system does not know how to shrink $OS_TO_STAGE"
	    SHRINK_ISO=true
	    shift;;
	--generate-minimal-install)
	    type generate_minimal_install &>/dev/null || \
		die "The build system does not know how to generate a minimal install list for $OS_TO_STAGE!"
	    GENERATE_MINIMAL_INSTALL=true
	    shift;;
	# If we need to perfoem a cache update, die insted.
	# This also has the build system ensure that any chroots
	# can pull packages straight out of the build cache, allowing
	# for fully offline builds.
	--no-cache-update) shift; ALLOW_CACHE_UPDATE=false;;
	--no-metadata-update) shift; ALLOW_CACHE_METADATA_UPDATE=false;;
        --wild-cache) shift; ALLOW_CACHE_UPDATE=true; ALLOW_CACHE_METADATA_UPDATE=true;
            WILD_CACHE=true
            export CACHE_DIR="$(mktemp -d "${CACHE_DIR%/*}/.crowbar_temp_cache-XXXXXX")"
            export SLEDGEHAMMER_PXE_DIR="$CACHE_DIR/tftpboot";;
	# Go through all the motions, but do not actaully generate
	# an ISO at the end.  This is useful for generating barclamp
	# tarballs.
	--no-iso) shift; NO_GENERATE_ISO=true;;
	--skip-lock) shift; __skip_lock=true;;
        --do-not-clean) shift; NO_CLEAN_DIRS=true;;
        --native-packages) shift; BUILD_NATIVE_PACKAGES=true;;
	*)          die "Unknown command line parameter $1";;
    esac
done

if [[ $BUILD_NATIVE_PACKAGES = true || \
    -f $CROWBAR_DIR/extra/install-crowbar-native.sh ]]; then
    debug "Building native OS packages"
    case $OS_TO_STAGE in
        ubuntu*|debian*) BC_PKG_TYPE="deb";;
        redhat*|centos*|suse*|opensuse*) BC_PKG_TYPE="rpm";;
    esac
fi

do_crowbar_build() {
    # Make sure only one instance of the ISO build runs at a time.
    # Otherwise you can easily end up with a corrupted image.
    # Check and see if our local build repository is a git repo. If it is,
    # we may need to do the same sort of merging in it that we might do in the
    # Crowbar repository.
    if [[ -d $CACHE_DIR/.git ]] && \
	(cd "$CACHE_DIR"; branch_exists master) then
	CURRENT_CACHE_BRANCH=master
    fi

    if [[ $BRANCHES_TO_MERGE ]]; then
	THROWAWAY_BRANCH="build-throwaway-$$-$RANDOM"
	REPO_PWD="$PWD"
	in_repo git checkout -b "$THROWAWAY_BRANCH"
	for br in "${BRANCHES_TO_MERGE[@]}"; do

	    # Merge the requested branch into the throwaway branch.
	    # Die if the merge failed -- there must have been a
	    # conflict, and the user needs to fix it up.
	    in_repo git merge "$1" || \
		die "Merge of $1 failed, fix things up and continue"
	done
	unset br
    fi
    # Finalize where we expect to find our caches and out chroot.
    # If they were set in one of the conf files, don't touch them.

    # The directory we perform a minimal install into if we need
    # to refresh our gem or pkg caches
    [[ $CHROOT ]] || CHROOT="$CACHE_DIR/$OS_TOKEN/chroot"

    # Make sure that the $OS_TOKEN directory exist.
    mkdir -p "$CACHE_DIR/$OS_TOKEN"

    # The directory we will stage the build into.
    [[ $BUILD_DIR ]] || \
	BUILD_DIR="$CACHE_DIR/$OS_TOKEN/build"
    # The directory that we will mount the OS .ISO on .
    [[ $IMAGE_DIR ]] || \
	IMAGE_DIR="$CACHE_DIR/$OS_TOKEN/image"

    # Directory where we will look for our package lists
    [[ $PACKAGE_LISTS ]] || PACKAGE_LISTS="$BUILD_DIR/extra/packages"

    # Proxy Variables
    [[ $USE_PROXY ]] || USE_PROXY=0
    [[ $PROXY_HOST ]] || PROXY_HOST=""
    [[ $PROXY_PORT ]] || PROXY_PORT=""
    [[ $PROXY_USER ]] || PROXY_USER=""
    [[ $PROXY_ESC_USER ]] || PROXY_ESC_USER=""
    [[ $PROXY_PASSWORD ]] || PROXY_PASSWORD=""

    # Version for ISO
    [[ $VERSION ]] || VERSION="$(crowbar_version).dev"

    # Name of the built iso we will build
    [[ $BUILT_ISO ]] || BUILT_ISO="crowbar-${VERSION}.iso"

    # If we were not passed a list of barclamps to include,
    # pull in all of the ones declared as submodules.
    [[ $BARCLAMPS ]] || BARCLAMPS=($(barclamps_in_build))
    # Pull in barclamp information
    get_barclamp_info

    # Make any directories we don't already have
    for d in "$ISO_LIBRARY" "$ISO_DEST" "$IMAGE_DIR" "$BUILD_DIR" \
	"$SLEDGEHAMMER_PXE_DIR" "$CHROOT"; do
	mkdir -p "$d"
    done

    debug "Checking for Sledgehammer."
    # Make sure Sledgehammer has already been built and pre-staged.
    if ! [[ -f $SLEDGEHAMMER_PXE_DIR/initrd0.img ]]; then
	debug "Slegehammer TFTP image missing!"
	debug "Attempting to build Sledgehammer:"
	"$CROWBAR_DIR/build_sledgehammer.sh" || \
	    die "Unable to build Sledgehammer. Cannot build Crowbar."
    fi

    # Fetch the OS ISO if we need to.
    [[ -f $ISO_LIBRARY/$ISO ]] || fetch_os_iso

    # Start with a clean slate.
    clean_dirs "$IMAGE_DIR" "$BUILD_DIR" "$CHROOT"

    if [[ $NO_CLEAN_DIRS = true ]]; then
        debug "Skipping clean by user request."
    else
        debug "Cleaning up any VCS cruft."
        # Clean up any cruft that the editor may have left behind.
        (for d in "$CROWBAR_DIR" "$CROWBAR_DIR/barclamps/"*; do
	    cd "$d"; $VCS_CLEAN_CMD
	    done)
    fi

    # Make additional directories we will need.
    for d in discovery extra/pkgs extra/files doc/framework; do
	mkdir -p "$BUILD_DIR/$d"
    done

    # Mount our ISO for the build process.
    debug "Mounting $ISO"
    sudo mount -t iso9660 -o loop "$ISO_LIBRARY/$ISO" "$IMAGE_DIR" || \
	die "Could not mount $ISO"
    debug "Indexing CD package pool."
    index_cd_pool

    # Copy over the Crowbar bits and their prerequisites
    # The ordering here is designed to always have the most specific
    # version of the file for a build wind up staged on the DVD, without
    # having to walk the whole directory tree.
    d="$(build_cfg_dir)" && [[ -d $d/extra && -d $d/change-image ]] || \
        die "Cannot find extra and change-image directories for $(current_build)!"
    cp -r "$d/extra"/* "$BUILD_DIR/extra"
    cp -r "$CROWBAR_DIR/doc"/* "$BUILD_DIR/doc/framework"
    cp -r "$d/change-image/"* "$BUILD_DIR"
    for d in "$OS-common" "$OS_TOKEN-extra"; do
	[[ -d $CROWBAR_DIR/$d ]] || continue
	cp -r "$CROWBAR_DIR/$d"/* "$BUILD_DIR/extra"
    done

    # Add critical build meta information to build-info
    echo "build-timestamp: $(date '+%F %T %z')" > "$BUILD_DIR/build-info"
    echo "build-os: $OS_TOKEN" >>"$BUILD_DIR/build-info"
    echo "build-os-iso: $ISO" >>"$BUILD_DIR/build-info"
    echo "crowbar: $(get_rev "$CROWBAR_DIR")" >>"$BUILD_DIR/build-info"

    # Make sure that all our barclamps are properly staged.
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
	    if $checker "$bc"; then
		[[ $ALLOW_CACHE_UPDATE = true ]] || {
		    echo "Need up update $cache cache for $bc, but updates are disabled."
		    echo "Please rerun the build with the --update-cache option."
		    exit 1
		} >&2
		debug "Updating $cache cache for $bc"
		[[ $cache =~ ^(pkg|gem)$ ]] && make_chroot
                MAYBE_UPDATE_GIT_CACHE=true
		$updater "$bc"
	    fi
	done
	# Handle building any requests if we call for a custom build
	if [[ ${BC_BUILD_CMDS["$bc"]} ]]; then
	    [[ -x $CROWBAR_DIR/barclamps/$bc/${BC_BUILD_CMDS["$bc"]%% *} ]] || \
		die "Asked to do a custom build for $bc, but build script ${BC_BUILD_CMDS["$bc"]%% *} not found!"
	    # Make sure the actual build runs in a subshell.  This prevents
	    # cross-barclamp namespace collisions.
	    (   export BC_DIR=$CROWBAR_DIR/barclamps/$bc
		export BC_CACHE=$CACHE_DIR/barclamps/$bc
		. "$BC_DIR"/${BC_BUILD_CMDS["$bc"]}
		if bc_needs_build; then
		    # Make sure we have a chroot set up and that it is ready
		    # to do whatever bc_build needs.
		    make_chroot
		    bind_mount "$CACHE_DIR/barclamps/$bc" "$CHROOT/mnt"
		    install_build_packages "$bc"
		    in_chroot ln -s /mnt/$OS_TOKEN /mnt/current_os
		    bc_build || die "External builder for $bc failed"
		    in_chroot rm -f /mnt/current_os
		    sudo umount "$CHROOT/mnt"
		fi
	    )
	fi
	make_barclamp_pkg_metadata "$bc"
        # Once all our barclamps have had their packages staged, create tarballs of them.
        mkdir -p "$BUILD_DIR/dell/barclamps"
        package_opts=(--destdir "$BUILD_DIR/dell/barclamps" --os "$OS_TOKEN")
        case $BC_PKG_TYPE in
            tar) : ;;
            deb) package_opts+=("--deb");;
            rpm) package_opts+=("--rpm");;
        esac
	echo "barclamps/$bc: $(get_rev "$CROWBAR_DIR/barclamps/$bc")" >> "$BUILD_DIR/build-info"
    done
    "$CROWBAR_DIR/package_barclamp.sh" "${package_opts[@]}" "${BARCLAMPS[@]}" || \
        die "Could not package $bc into a $BC_PKG_TYPE"

    [[ $NO_GENERATE_ISO = true ]] && return 0

    if [[ $ALLOW_CACHE_UPDATE != true && $CURRENT_CACHE_BRANCH ]]; then
	echo "build-cache: $(get_rev "$CACHE_DIR")" >> "$BUILD_DIR/build-info"
    fi

    (cd "$BUILD_DIR"
	find extra dell -type f -print | \
	    sort >> "build-info")
    # Make sure we still provide the legacy ami location
    (cd "$BUILD_DIR"; ln -sf extra/files/ami)
    # Store off the version
    echo "$VERSION" >> "$BUILD_DIR/dell/Version"

    # Custom start-up in place
    BUILD_CFG_DIR="$(build_cfg_dir)" && {
	for f in "$BUILD_CFG_DIR"/*.json ; do
	    [[ -f $f ]] || continue
	    mkdir -p "$BUILD_DIR/extra/config"
	    cp "$f" "$BUILD_DIR/extra/config"
	done
    }

    final_build_fixups

    # Copy over the bits that Sledgehammer will look for.
    debug "Copying over Sledgehammer bits"
    cp -a "$SLEDGEHAMMER_PXE_DIR"/* "$BUILD_DIR/discovery"

    # Make our image
    debug "Creating new ISO"
    # mkisofs can merge multiple directory trees into a single iso
    # file system.  However, these trees must be disjoint -- if
    # any of the filesystem trees collide, mkisofs dies.
    # To work around that, we merge any colliding top-level trees
    # and take care to prefer stuff from $BUILD_DIR, and then
    # bind-mount empty trees on top of the colliding trees in $IMAGE_DIR
    for d in $(cat <(cd "$BUILD_DIR"; find -maxdepth 1 -type d) \
	<(cd "$IMAGE_DIR"; find -maxdepth 1 -type d) | \
	sort |uniq -d); do
	[[ $d = . ]] && continue
	d=${d#./}
	# Copy contents of the found directories into $BUILD_DIR, taking care
	# to not clobber existing files.
	mkdir -p "$BUILD_DIR/$d"
	chmod u+wr "$BUILD_DIR/$d"
	# We could also use cp -n, but rhel5 and centos5 do not understand it.
	rsync -rl --ignore-existing --inplace "$IMAGE_DIR/$d" "$BUILD_DIR"
	chmod -R u+wr "$BUILD_DIR/$d"
	# Bind mount an empty directory on the $IMAGE_DIR instance.
	sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/$d"
    done
    mkdir -p "$BUILD_DIR/isolinux"
    chmod u+wr "$BUILD_DIR/isolinux"
    rsync -rl --ignore-existing --inplace \
	"$IMAGE_DIR/isolinux" "$BUILD_DIR"
    chmod -R u+wr "$BUILD_DIR/isolinux"
    sudo mount -t tmpfs -o size=1K tmpfs "$IMAGE_DIR/isolinux"

    [[ $SHRINK_ISO && ! $GENERATE_MINIMAL_ISO ]] && shrink_iso
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
    build_iso || die "There was a problem building our ISO."
    if [[ $GENERATE_MINIMAL_INSTALL = true ]]; then
	if [[ ! -f "$CROWBAR_DIR/$OS_TOKEN-extra/minimal-install" ]]; then
	    if [[ ! -f "$HOME/admin-installed.list" ]]; then
		SMOKETEST_ISO="$ISO_DEST/$BUILT_ISO"
		test_iso admin-only
	    fi
	    [[ -f "$HOME/admin-installed.list" ]] || \
		die "Could not generate minimal install list!"
	    mv "$HOME/admin-installed.list" \
		"$CROWBAR_DIR/$OS_TOKEN-extra/minimal-install"
	    debug "Minimal install generated and saved to $CROWBAR_DIR/$OS_TOKEN-extra/minimal-install."
	    debug "Please commit it and rerun the build with --shrink."
	fi
    fi
    echo "$(date '+%F %T %z'): Image at $ISO_DEST/$BUILT_ISO"
    if [[ $NEED_TEST = true ]]; then
	echo "$(date '+%F %T %z'): Testing new iso"
	SMOKETEST_ISO="$ISO_DEST/$BUILT_ISO"
	test_iso "${test_params[@]}" && \
	    echo "$(date '+%F %T %z'): Test passed" || \
	    die "Test failed."
    fi
    echo "$(date '+%F %T %z'): Finished."
}

if [[ $__skip_lock = true ]]; then
    do_crowbar_build
else
    with_build_lock do_crowbar_build
fi
