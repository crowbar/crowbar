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
### The cluster hash is shared among all nodes in a swift cluster.
### can be generated using od -t x8 -N 8 -A n </dev/random
default[:swift][:cluster_hash]="fa8bea159b55bd7e"
### super user password - used for managing users.
default[:swift][:cluster_admin_pw]= "swauth"
### how many replicas should be made for each object
default[:swift][:replicas]= 1
## how many zones are in this cluster (should be >= # of replicas)
default[:swift][:zones]= 2
## minimum amount of time a partition should stay put, in hours
default[:swift][:min_part_hours]= 1
## number of bits to represent the partitions count 
default[:swift][:partitions]= 18

### the uid/gid to be used for swift processes
default[:swift][:user]= "swift"
default[:swift][:group]= "swift"


default[:swift][:config] = {}
default[:swift][:config][:environment] = "default"

### where to find IP for admin use
default[:swift][:admin_ip_expr] = "node[:ipaddress]" 
### where to find IP for admin use
default[:swift][:storage_ip_expr] = "node[:ipaddress]" 

# expression to find a hash of possible disks to be used.
default[:swift][:disk_enum_expr]= 'node[:block_device]'
# expression accepting a k,v pair for evaluation. if expression returns true, then the disk will be used.
# by default, use any sdX or hdX that is not the first one (which will hold the OS).
default[:swift][:disk_test_expr]= 'k =~/sd[^a]/ or k=~/hd[^a]/'      

# An expression to classify disks into zone's and assign them a weight.
# return 
#   - nil: the disk is not included in the ring
#   - otherwise an array of [zone, weight]. Zone is an integer representing the zone # for the disk is expected, weight is the weight of the disk  
# the default expression below just assigns disks in a round robin fashion.
#
# The expression is evaluated with the following context:
# - node - the Chef node hash
# - params - a hash with the following keys:
#   :ring=> one of "object", "account" or "container"
#   :disk=> disk partition information as created in disks.rb,contains: :name (e.g sdb) :size either :remaining (= all the disk) or an actual byte count.           
default[:swift][:disk_zone_assign_expr] = '$DISK_CNT||=0; $DISK_CNT= $DISK_CNT+1 ;[ $DISK_CNT % node[:swift][:zones] , 99]'
