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
dest=/opt/dell/openstack_manager/public/crowbar-cli.tar.gz
sedopts=()
[[ $1 ]] && sedopts+=(-e "/^@hostname/ s/127.0.0.1/$1/")
[[ $2 ]] && sedopts+=(-e "/^@port/ s/3000/$2/")

[[ -f $dest ]] && rm -f "$dest"

loc=$(mktemp -d /tmp/crowbar-cli-XXXXXX)
[[ -d $loc ]] || exit 1

for f in /opt/dell/bin/*; do
    [[ -f $f ]] || continue
    sed "${sedopts[@]}" "$f" >"$loc/${f##*/}"
done

cd "${loc}"
cp /tftpboot/ubuntu_dvd/extra/gems/net-http-digest*.gem .
cat >install.sh <<"EOF"
#!/bin/bash

[[ $1 && -d $1 && -w $1 ]] || {
    echo "Please pass a directory to install the Crowbar CLI to."
    exit 1
}
gem install net-http-digest_auth --no-ri --no rdoc --local net-http-*.gem || {
    echo "Could not install the net-http-digest_auth gem."
    echo "Make sure gem is installed, and you have permissions to install gems."
    exit 1
}
rm *.gem
cp * "$1"

EOF
chmod 755 *
tar czf "$dest" crowbar* barclamp* *.sh *.gem
cd /
rm -rf "${loc}"