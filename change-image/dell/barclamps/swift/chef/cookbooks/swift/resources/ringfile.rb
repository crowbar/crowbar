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


=begin
  A ring file describes the disks in a swift cluster.
  the utility used to manipulate it requires only changes to by applied

  Each disk needs to be described with the following:
  :ip - the IP address of the machine
  :port - the port the server on that machine is listening on
  :dev_name - the device's name
  :weight - a relative (to other disks) weight. determines how heavily used this disk will be
  :zone - the zone the disk is in

When 'applied'  this resource compares the current disk information in the ring file to that provided
to it, and adds the missing disks

Sample usage below. Note that disks_c is an array of hashes.

swift_ringfile "container.builder" do
  disks disks_c
  action [:apply, :rebalance]
end



=end

actions :apply, :rebalance
attribute :disks 
attribute :partitions, :kind_of => Integer
attribute :replicas, :kind_of => Integer
attribute :min_part_hours, :kind_of => Integer 
