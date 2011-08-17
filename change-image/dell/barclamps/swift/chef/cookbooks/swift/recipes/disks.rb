#
# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: andi abes
#

package "xfsprogs"

log("locating disks using #{node[:swift][:disk_enum_expr]} test: #{node[:swift][:disk_test_expr]}") {level :debug}
to_use_disks = {}
all_disks = eval(node[:swift][:disk_enum_expr])
all_disks.each { |k,v|
  b = binding()
  to_use_disks[k]=v if eval(node[:swift][:disk_test_expr])  
}

log("will use these disks: #{to_use_disks.keys.join(':')}") {level :debug}

node[:swift][:devs] = []
to_use_disks.each { |k,v| 

  target_suffix= k + "1" # by default, will use format first partition.
  target_dev = "/dev/#{k}"
  target_dev_part = "/dev/#{target_suffix}"

  # protect against OS's that confuse ohai. if the device isnt there.. don't 'try to use it.
  if File.exists?(target_dev) == false
    log ("device: #{target_dev} doesn't seem to exist. ignoring") {level :warn }
    next
  end
  
  swift_disk "/dev/#{k}" do
    part [{ :type => "xfs", :size => :remaining} ]
    action :ensure_exists
  end
  
  
  execute "make xfs filesystem on #{k}" do
    command "mkfs.xfs -f -i size=1024 #{target_dev}"
    ## test if the FS is already an XFS file system.
    not_if "xfs_admin -l #{target_dev}"
  end
  
  directory "/srv/node/#{target_suffix}" do
    group "swift"
    owner "swift"
    recursive true
    action :create
  end
  
  mount "/srv/node/#{target_suffix}"  do  
    device target_dev
    options "noatime,nodiratime,nobarrier,logbufs=8"
    dump 0  
    fstype "xfs"
    action [:mount, :enable]
  end  
  #### 
  # publish the disks
  node[:swift][:devs] <<  {:name=>target_suffix, :size=> :remaining }   
}
