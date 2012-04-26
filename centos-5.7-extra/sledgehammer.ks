lang en_US.UTF-8
keyboard us
timezone US/Eastern
auth --useshadow --enablemd5
selinux --disabled
firewall --disabled

repo --name=a-base    --baseurl=http://mirror.centos.org/centos/5/os/$basearch
repo --name=a-updates --baseurl=http://mirror.centos.org/centos/5/updates/$basearch
repo --name=a-extras  --baseurl=http://mirror.centos.org/centos/5/extras/$basearch
repo --name=a-epel    --baseurl=http://mirror.pnl.gov/epel/5/$basearch
repo --name=a-live    --baseurl=http://www.nanotechnologies.qc.ca/propos/linux/centos-live/$basearch/live
repo --name=a-rbel    --baseurl=http://rbel.frameos.org/stable/el5/$basearch

%packages
bash
kernel
syslinux
passwd
policycoreutils
chkconfig
authconfig
rootfiles
comps-extras
xkeyboard-config
dhclient
tcpdump
vim-enhanced
openssh-clients
openssh-server
OpenIPMI-tools
OpenIPMI
portmap
nfs-utils
procmail
wget
lshw
dmidecode
glibc.i686
libxml2
libxml2-devel
zlib
zlib-devel
libxslt
libxml2.i386
compat-libstdc++-33.i386
pciutils
ntp
which
rpm
coreutils
tar
gzip
mktemp
libsysfs.x86_64
libsysfs.i386 
yum
curl
ruby
ruby-libs.x86_64
ruby-devel.x86_64
ruby-ri
ruby-rdoc
rubygems
rubygem-chef
rubygem-ohai
git 
gcc
gcc-c++
automake
autoconf
make
perl-XML-Twig
parted
lvm2

%post

yum -y remove '*.i?86'

########################################################################
# Create a sub-script so the output can be captured
# Must change "$" to "\$" and "`" to "\`" to avoid shell quoting
########################################################################
cat > /root/post-install << EOF_post
#!/bin/bash

echo ###################################################################
echo ## Creating the centos-live init script
echo ###################################################################

cat > /etc/rc.d/init.d/openstack-start-up << EOF_initscript
#!/bin/bash
#
# live: Init script for live image
#
# chkconfig: 345 72 28
# description: Init script for live image.


EOF_initscript

/sbin/chkconfig --add openstack-start-up

EOF_post

/bin/bash -x /root/post-install 2>&1 | tee /root/post-install.log


%post --nochroot

########################################################################
# Create a sub-script so the output can be captured
# Must change "$" to "\$" and "`" to "\`" to avoid shell quoting
########################################################################
cat > /root/postnochroot-install << EOF_postnochroot
#!/bin/bash

cp *.gem $INSTALL_ROOT/root

cp start-up.sh $INSTALL_ROOT/etc/rc.d/init.d/openstack-start-up
chmod +x $INSTALL_ROOT/etc/rc.d/init.d/openstack-start-up
cp sshd_config $INSTALL_ROOT/etc/ssh/sshd_config

cp dhclient.conf $INSTALL_ROOT/etc

EOF_postnochroot

/bin/bash -x /root/postnochroot-install 2>&1 | tee /root/postnochroot-install.log

