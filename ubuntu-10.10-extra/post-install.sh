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

#
# This script is called by the ubuntu.seed file during
# ubuntu install.  It handles doing the manual sets needed to layout the 
# filesystem the rest of the way before running the common_install.sh.
#
# The intent is that once the system is finished installing, the customer
# will edit the config.json file and run install.sh to complete installation
# and initial configuration.
#

#
# We are a cd install. 
#

mkdir -p /target/tftpboot/ubuntu_dvd/extra
cp /cdrom/extra/common_install.sh /target/tftpboot/ubuntu_dvd/extra
cd /cdrom
cp -a . /target/tftpboot/ubuntu_dvd

mount /proc /target/proc
chroot /target bash /tftpboot/ubuntu_dvd/extra/common_install.sh

