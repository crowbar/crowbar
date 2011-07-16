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

class TestService < ServiceObject

  def initialize(thelogger)
    @bc_name = "test"
    @logger = thelogger
  end

  def create_proposal
    @logger.debug("Test create_proposal: entering")
    base = super
    @logger.debug("Test create_proposal: leaving base part")

    nodes = NodeObject.find_nodes_by_name "dtest*"
    nodes = nodes.sort{|a, b| a.name <=> b.name}

    if nodes.size == 1
      base["deployment"]["test"]["elements"] = {
        "test-single" => [ nodes.first.name ]
      }
    elsif nodes.size > 1
      head = nodes.shift
      base["deployment"]["test"]["elements"] = {
        "test-multi-head" => [ head.name ],
        "test-multi-rest" => nodes.map { |x| x.name }
      }
    end

    @logger.debug("Test create_proposal: exiting")
    base
  end

end

