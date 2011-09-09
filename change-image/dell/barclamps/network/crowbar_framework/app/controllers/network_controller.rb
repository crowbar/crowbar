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

class NetworkController < BarclampController
  # Make a copy of the barclamp controller help
  self.help_contents = Array.new(superclass.help_contents)
  def initialize
    @service_object = NetworkService.new logger
  end

  # Below here handles ip address assignment.
  add_help(:allocate_ip,[:id,:network,:range,:name],[:post])
  def allocate_ip
    id = params[:id]       # Network id
    network = params[:network]
    range = params[:range]
    name = params[:name]

    ret = @service_object.allocate_ip(id, network, range, name)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  # Below here handles ip address assignment.
  add_help(:enable_interface,[:id,:network,:name],[:post])
  def enable_interface
    id = params[:id]       # Network id
    network = params[:network]
    name = params[:name]

    ret = @service_object.enable_interface(id, network, name)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

end

