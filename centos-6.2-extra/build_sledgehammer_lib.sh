#!/bin/bash

# This file contains library routines needed to build Sledgehammer

EXTRA_REPOS=('http://mirror.centos.org/centos/6/os/$basearch' \
    'http://mirror.centos.org/centos/6/updates/$basearch' \
    'http://mirror.centos.org/centos/6/extras/$basearch' \
    'http://mirrors.xmission.com/fedora/epel/6/$basearch' \
    'http://mirrors.xmission.com/fedora/epel/6/$basearch' \
    'http://www.nanotechnologies.qc.ca/propos/linux/centos-live/$basearch/live' \
    'http://rbel.frameos.org/stable/el6/$basearch')

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
    chroot_install livecd-tools tar
}
