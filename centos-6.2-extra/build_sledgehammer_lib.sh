#!/bin/bash

# This file contains library routines needed to build Sledgehammer

EXTRA_REPOS=('http://mirror.centos.org/centos/6/os/x86_64' \
    'http://mirror.centos.org/centos/6/updates/x86_64' \
    'http://mirror.centos.org/centos/6/extras/x86_64' \
    'http://mirror.pnl.gov/epel/6/x86_64' \
    'http://www.nanotechnologies.qc.ca/propos/linux/centos-live/x86_64/live' \
    'http://rbel.frameos.org/stable/el6/x86_64')

setup_sledgehammer_chroot() {
    local repo rnum
    local packages=() pkg
    local files=() file
    local mirror="${EXTRA_REPOS[0]}"
    local -A base_pkgs
    # Build a hash of base packages. We will use this to track the packages we found in the mirror.
    for pkg in "${OS_BASIC_PACKAGES[@]}"; do
        base_pkgs["$pkg"]="needed"
    done
    # Fourth, get a list of packages in the mirror that we will use.
    match_re='^([A-Za-z0-9._+-]+)-([0-9]+:)?([0-9a-zA-Z._]+)-([^-]+)(\.el6.*)?\.(x86_64|noarch)\.rpm'
    while read file; do
        # Do we actaully care at all about this file?
        [[ $file =~ $match_re ]] || continue
        # Is this a file we need to download?
        [[ ${base_pkgs["${BASH_REMATCH[1]}"]} ]] || continue
        # It is. Mark it as found and put it in the list.
        base_pkgs["${BASH_REMATCH[1]}"]="found"
        files+=("-O" "${mirror}/Packages/$file")
    done < <(curl -sfL "{$mirror}/Packages/" | \
        sed -rn 's/.*"([^"]+\.(x86_64|noarch).rpm)".*/\1/p')
    # Fifth, make sure we found all our packages.
    for pkg in "${base_pkgs[@]}"; do
        [[ $pkg = found ]] && continue
        die "Not all files for CentOS chroot found."
    done
    # Sixth, suck all of our files and install them in one go
    sudo mkdir -p "$CHROOT"
    (
        set -e
        set -o pipefail
        cd "$CHROOT"
        debug "Fetching files needed for chroot"
        curl -sfL "${files[@]}" || exit 1
        for file in *.rpm; do
            debug "Extracting $file"
            rpm2cpio "$file" | sudo cpio --extract --make-directories \
                --no-absolute-filenames --preserve-modification-time &>/dev/null
            if [[ $file =~ (centos|redhat)-release ]]; then
                sudo mkdir -p "$CHROOT/tmp"
                sudo cp "$file" "$CHROOT/tmp/${file##*/}"
                postcmds+=("/bin/rpm -ivh --force --nodeps /tmp/${file##*/}")
            fi
            rm "$file"
        done
        # Seventh, fix up the chroot so that it is fully functional.
        sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"
        for d in /proc /sys /dev /dev/pts /dev/shm; do
            [[ -L $d ]] && d="$(readlink -f "$d")"
            mkdir -p "${CHROOT}$d"
            sudo mount --bind "$d" "${CHROOT}$d"
        done
        # Eighth, run any post cmds we got earlier
        for cmd in "${postcmds[@]}"; do
            in_chroot $cmd
        done
    ) || die "Not all files needed for CentOS chroot downloaded."
    sudo rm -f "$CHROOT/etc/yum.repos.d/"*
    rnum=0
    for repo in "${EXTRA_REPOS[@]}"; do
        add_repos "bare r${rnum} 10 $repo"
        rnum=$((rnum + 1))
    done
    # Eleventh, bootstrap the rest of the chroot with yum.
    in_chroot yum -y install yum yum-downloadonly createrepo
    # fastestmirror support behind a proxy is not that good.
    [[ -f $CHROOT/etc/yum/pluginconf.d/fastestmirror.conf ]] && \
        in_chroot sed -ie '/^enabled/ s/1/0/' \
        /etc/yum/pluginconf.d/fastestmirror.conf
    # Make sure yum does not throw away our caches for any reason.
    in_chroot /bin/sed -i -e '/keepcache/ s/0/1/' /etc/yum.conf
    in_chroot sh -c "echo 'exclude = *.i386' >>/etc/yum.conf"
    # fourth, have yum bootstrap everything else into usefulness
    chroot_install livecd-tools tar
}
