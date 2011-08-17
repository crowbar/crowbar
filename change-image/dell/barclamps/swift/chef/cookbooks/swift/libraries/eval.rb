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

require 'chef'

class Swift
  class Evaluator
    
    def initialize(node)
      @b = binding    
    end
    
    def eval_with_context(str)
      eval(str,@b)
    end
    
    def self.eval_with_params(str,node,params)
      b = binding
      b.eval(str)
    end
    
    def log_eval_vars()
      eval("Chef::Log.info('locals:'+local_variables.join(':') + '\nglobals:'+global_variables.join(':'))")
    end
    
    
    def self.get_ip_by_type(node, type)
      ip_location = node[:swift][type]
      e = Evaluator.new(node)
      ip = e.eval_with_context(ip_location)
      Chef::Log.debug("Looking at #{ip_location} for #{type} IP addr. Got: #{ip}")
      ip
    end
    
  end
end
