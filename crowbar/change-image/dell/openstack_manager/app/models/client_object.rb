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
# Author: RobHirschfeld 
# 
#
# Also functions as a data bag item wrapper as well.
#
class ClientObject < ChefObject

  def self.find_client_by_name(name)
    begin
      chef_init
      return ClientObject.new Chef::ApiClient.load(name)
    rescue Exception => e
      Rails.logger.fatal("Failed to find client: #{name} #{e.message}")
      return nil
    end
  end

  def initialize(client)
    @client = client
  end

  def save
    @client.save
  end

  def destroy
    @client.destroy
  end

end


