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
# 

# Install barclamps for now
cd "$1/chef"

cp -r * /opt/dell/chef

cd cookbooks; knife cookbook upload -o . -a; cd ..

cd data_bags/crowbar;
for i in *.json; do
    knife data bag from file crowbar $i
done 
cd ../.. # now in $1/chef again
cd roles
for i in *.rb; do
    knife role from file $i
done 
cd ../.. # now just in $1
cd app; cp -r * /opt/dell/crowbar_framework/app; cd ..
cd command_line; cp * /opt/dell/bin; cd ..
cp -r public /opt/dell/crowbar_framework;
  
if [[ -f /etc/redhat-release ]]; then
    service httpd reload
else
    service apache2 reload
fi

cd -

