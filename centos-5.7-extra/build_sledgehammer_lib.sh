#!/bin/bash

# This file contains lobrary routines needed to build Sledgehammer on Centos 5.7

EXTRA_REPOS=('http://mirror.centos.org/centos/5/os/$basearch' \
    'http://mirror.centos.org/centos/5/updates/$basearch' \
    'http://mirror.centos.org/centos/5/extras/$basearch' \
    'http://mirror.pnl.gov/epel/5/$basearch' \
    'http://www.nanotechnologies.qc.ca/propos/linux/centos-live/$basearch/live' \
    'http://rbel.frameos.org/stable/el5/$basearch')

setup_sledgehammer_chroot() {
    local repo rnum
    sudo rm -f "$CHROOT/etc/yum.repos.d/"*
    rnum=0
    for repo in "${EXTRA_REPOS[@]}"; do
        add_repos "bare r${rnum} 10 $repo"
        rnum=$((rnum + 1))
    done
    # Make sure yum does not throw away our caches for any reason.
    in_chroot /bin/sed -i -e '/keepcache/ s/0/1/' /etc/yum.conf
    in_chroot sh -c "echo 'exclude = *.i386' >>/etc/yum.conf"
    # fourth, have yum bootstrap everything else into usefulness
    in_chroot mkdir -p "/usr/lib/python2.4/site-packages/urlgrabber.broke"
    for f in "$CHROOT/usr/lib/python2.4/site-packages/urlgrabber/keepalive"*; do
        in_chroot mv "${f#$CHROOT}" \
            "/usr/lib/python2.4/site-packages/urlgrabber.broke/"
    done
    chroot_install livecd-tools rhpl kudzu
    # Force livecd-creator to use ext2 instead of ext3.
    in_chroot /bin/sed -i -e '/self.__fstype/ s/ext3/ext2/' \
        /usr/lib/python2.4/site-packages/imgcreate/creator.py

    in_chroot /bin/mv "/sbin/mksquashfs" "/sbin/mksquashfs.orig"
    in_chroot touch /sbin/mksquashfs
    in_chroot chmod 777 /sbin/mksquashfs
    cat > "$CHROOT/sbin/mksquashfs" <<"EOF"
#!/bin/bash

# nuke any -no-progress arg, leave the rest unchanged.
/sbin/mksquashfs.orig "${@//-no-progress/}" </dev/null 2>/dev/null >/dev/nul
EOF
    in_chroot /bin/chmod 777 /sbin/mksquashfs
    in_chroot /bin/chmod 777 /sbin/mksquashfs.orig
}