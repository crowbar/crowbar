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

dest=/opt/dell/crowbar_framework/public/crowbar-cli.tar.gz
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
for gem in net-http-digest json; do
    cp /tftpboot/ubuntu_dvd/extra/gems/${gem}*.gem .
done
cat >install.sh <<"EOF"
#!/bin/bash

[[ $1 && -d $1 && -w $1 ]] || {
    echo "Please pass a directory to install the Crowbar CLI to."
    exit 1
}
for gem in net-http-digest_auth json; do
    gem install $gem --no-ri --no rdoc --local ${gem}*.gem || {
        echo "Could not install the net-http-digest_auth gem."
        echo "Make sure gem is installed, and you have permissions to install gems."
        exit 1
    }
done
rm *.gem
cp * "$1"

EOF
chmod 755 *
tar czf "$dest" crowbar* barclamp* install.sh *.gem
cd /
rm -rf "${loc}"
