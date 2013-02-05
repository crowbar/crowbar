#!/bin/bash
# We always use the C language and locale
export LANG="C"
export LC_ALL="C"
BUILD_TYPE="tar"
readonly currdir="$PWD"
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin"

die() { echo "$*"; exit 1; }
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
	    [[ -d "$1" ]] || die "Cannot install generated packages in $1, it does not exist"
	    DEST="$1";;
	--os)
	    shift
	    OS_PKGS="$1";;
        --deb)
            [[ -d $CROWBAR_DIR/packaging/deb ]] || die "Cannot build .deb packages!"
            BUILD_TYPE=deb;;
        --rpm)
            [[ -d $CROWBAR_DIR/packaging/rpm ]] || die "Cannot build .rpm packages!"
            BUILD_TYPE=rpm;;
        --no-cache)
            NO_CACHE=true;;
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
    bc=${barclamp##*/}
    if [[ -d $CACHE_DIR/barclamps/$bc && ! $NO_CACHE ]]; then
	mkdir -p "$bc/cache"
	if [[ $OS_PKGS ]]; then
	    for d in files gems "$OS_PKGS"; do
		[[ -d $CACHE_DIR/barclamps/$bc/$d ]] || continue
		[[ -L $d ]] || ln -s -t "$bc/cache" "$CACHE_DIR/barclamps/$bc/$d"
	    done
	else
	    for d in "$CACHE_DIR/barclamps/$bc"/*; do
		[[ -d $d ]] || continue
		[[ -L $d ]] || ln -s -t "$bc/cache/" "$d"
	    done
	fi
        tar chf - "$bc" |gzip -9 >"$DEST/$bc-cache.tar.gz"
        find "$bc/cache" -type l -print0 |xargs -0 rm -f --
        rm -rf "$bc/cache"
    fi
    cp -a "$barclamp" .
    (
	cd "$tmpdir/$bc"
        if [[ -d .git ]]; then
            GIT_COMMIT=$(git log -n 1 | grep "^commit" | awk -F" " '{ print $2 }')
            GIT_DATE=$(git log -n 1 | grep "^Date:" | sed 's/Date:[ ]*//')
            echo "" >> crowbar.yml
            echo "git:" >> crowbar.yml
            echo "  date: $GIT_DATE" >> crowbar.yml
            echo "  commit: $GIT_COMMIT" >> crowbar.yml
            rm -rf -- .git
        fi
        # Generate our sha1sums
        find -L -type f -not -name sha1sums -print0 | \
	    xargs -0 sha1sum -b >sha1sums
    )
    case $BUILD_TYPE in
        tar) tar chf - "$bc" |gzip -9 >"$DEST/$bc-barclamp.tar.gz";;
        deb) cp -a "$CROWBAR_DIR/packaging/deb/debian" "$tmpdir/$bc"
            # Make a potentially hideous changelog
            (   cd "$CROWBAR_DIR/barclamps/$bc"
                git log --pretty=format:"crowbar-barclamp-$bc (%ct.%h) unstable; urgency=low%n%n  * %s%n%n -- %an <%ae>  %aD%n" \
                    --no-merges >"$tmpdir/$bc/debian/changelog"
            )
            "$CROWBAR_DIR/packaging/make-pkg" "$tmpdir/$bc" "$tmpdir" deb || \
                die "Cannot create .deb for $bc"
                find "$tmpdir" -name "crowbar-barclamp-$bc_*.deb" \
                    -exec mv '{}' "$DEST" ';';;
        rpm) mkdir -p "$tmpdir/BUILD" "$tmpdir/RPMS" 
            "$CROWBAR_DIR/packaging/make-pkg" "$tmpdir/$bc" "$tmpdir" rpm || \
            die "Cannot create .rpm for $bc"
            find "$tmpdir/RPMS" -name "crowbar-barclamp-$bc-*.rpm" \
                -exec mv '{}' "$DEST" ';';;
        *) die "Unknown package type $BUILD_TYPE";;
    esac
done


