#!/bin/bash
# We always use the C language and locale
export LANG="C"
export LC_ALL="C"

readonly currdir="$PWD"
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin"

package_cleanup() {
    [[ $tmpdir && -d $tmpdir ]] || return 0
    find "$tmpdir" -type l -print0 |xargs -0 rm -f --
    rm -rf -- "$tmpdir"
}

trap package_cleanup 0 INT QUIT TERM
if [[ ! $CACHE_DIR ]]; then

    # Source our config file if we have one
    [[ -f $HOME/.build-crowbar.conf ]] && \
        . "$HOME/.build-crowbar.conf"

    # Look for a local one.
    [[ -f build-crowbar.conf ]] && \
        . "build-crowbar.conf"
fi

# Location for caches that should not be erased between runs
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.crowbar-build-cache"

# Location of the Crowbar checkout we are building from.
[[ $CROWBAR_DIR ]] || CROWBAR_DIR="${0%/*}"
[[ $CROWBAR_DIR = /* ]] || CROWBAR_DIR="$currdir/$CROWBAR_DIR"
[[ -f $CROWBAR_DIR/build_crowbar.sh && -d $CROWBAR_DIR/.git ]] || \
    die "$CROWBAR_DIR is not a git checkout of Crowbar!" 
export CROWBAR_DIR

BARCLAMPS=()
DEST="$currdir"

while [[ $1 ]]; do
    case $1 in
	--destdir)
	    shift
	    [[ -d "$1" ]] || die "Cannot install generated tarballs in $1, it does not exist"
	    DEST="$1";;
	--os)
	    shift
	    OS_PKGS="$1";;
	*)
	    if [[ -f $1/crowbar.yml ]]; then
		[[ $1 = /* ]] && BARCLAMPS+=("$1") || \
		    BARCLAMPS+=("$currdir/$1")
	    elif [[ -f $CROWBAR_DIR/barclamps/$1/crowbar.yml ]]; then
		BARCLAMPS+=("$CROWBAR_DIR/barclamps/$1")
	    else
		die "$1 is not a barclamp!"
	    fi ;;
	    
    esac
    shift
done

tmpdir="$(mktemp -d /tmp/package-bk-XXXXX)"
cd "$tmpdir"
for barclamp in "${BARCLAMPS[@]}"; do
    cp -a "$barclamp" .
    (   bc=${barclamp##*/}
	cd "$tmpdir/$bc"
    [[ -d .git ]] && GIT_COMMIT=$(git log -n 1 | grep "^commit" | awk -F" " '{ print $2 }')
    [[ -d .git ]] && GIT_DATE=$(git log -n 1 | grep "^Date:" | sed 's/Date:[ ]*//')
    echo "" >> crowbar.yml
    echo "git:" >> crowbar.yml
    echo "  date: $GIT_DATE" >> crowbar.yml
    echo "  commit: $GIT_COMMIT" >> crowbar.yml
	[[ -d .git ]] && rm -rf -- .git
	[[ -d $CACHE_DIR/barclamps/$bc ]] && (
	    mkdir -p cache
	    cd cache
	    if [[ $OS_PKGS ]]; then
		for d in files gems "$OS_PKGS"; do
		    [[ -d $CACHE_DIR/barclamps/$bc/$d ]] || continue
		    [[ -L $d ]] || ln -s "$CACHE_DIR/barclamps/$bc/$d" .
		done
	    else
		for d in "$CACHE_DIR/barclamps/$bc"/*; do
		    [[ -d $d ]] || continue
		    [[ -L $d ]] || ln -s "$d" .
		done
	    fi
	)
	find -L -type f -not -name sha1sums -print0 | \
	    xargs -0 sha1sum -b >sha1sums
	(cd ..; tar chf - "$bc" |gzip -9 >"$DEST/$bc.tar.gz")
    )
done
