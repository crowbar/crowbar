#!/bin/bash

BASEDIR="/tftpboot/ubuntu_dvd"
# copy the install image.
mkdir -p "$BASEDIR"
(   cd "$BASEDIR"
    while ! wget -q http://192.168.1.2:8091/files.list; do sleep 1; done
    while read f; do
	wget -a /root/post-install-wget.log -x -nH --cut-dirs=1 \
	    "http://192.168.1.2:8091/${f#./}"
    done < files.list
    rm files.list
)

# Fix links
while read file dest; do
  L_FILE=${file##*/}
  L_DIR=${file%/*}
  T_FILE=$dest
  (cd "${BASEDIR}/$L_DIR" ; ln -s "$T_FILE" "$L_FILE")
done < "${BASEDIR}/crowbar_links.list"

. "$BASEDIR/extra/common_install.sh"