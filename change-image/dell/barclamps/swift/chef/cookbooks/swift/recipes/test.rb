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
require 'rubygems'
require 'json'
require 'net/http'


t = <<END 
{
  "id": "bc-template-swift",
  "description": "The default proposal for swift",
  "attributes": {
    "swift": {
    "cluster_hash": "fa8bea159b55bd7e",
    "cluster_admin_pw": "swauth",
    "replicas": "1"   
    }
  },
  "deployment": {
    "crowbar-revision": 0,
    "swift": {
      "elements": {},
      "element_order": [
        [ "swift-storage", "swift-proxy"],
    [ "swift-ring-compute"]
      ],
      "config": {
        "environment": "swift-config-base",
        "mode": "full",
        "transitions": false,
        "transition_list": []
      }
    }
  }
}
END


JSON.parse(t)

def _eval(s,h)
  @b = binding
  @b.eval(s)
end

s = 'local_variables.each { |v| puts v }; global_variables.each { |g| puts g} '

puts _eval(s,:a =>'foo', :b=>'c')

