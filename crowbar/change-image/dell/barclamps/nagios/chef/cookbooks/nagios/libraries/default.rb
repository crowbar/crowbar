#
# Author:: Joshua Sierles <joshua@37signals.com>
# Cookbook Name:: nagios
# Library:: default
#
# Copyright 2009, 37signals
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
def nagios_boolean(true_or_false)
  true_or_false ? "1" : "0"
end

def nagios_interval(seconds)
  if seconds.to_i < node[:nagios][:interval_length].to_i
    raise ArgumentError, "Specified nagios interval of #{seconds} seconds must be equal to or greater than the default interval length of #{node[:nagios][:interval_length]}"
  end
  interval = seconds / node[:nagios][:interval_length]
  interval
end

def nagios_attr(name)
  node[:nagios][name]
end

class Nagios
class Evaluator
  
  def initialize(node)
    @b = binding    
  end
  
  def eval_with_context(str)
    eval(str,@b)
  end
  
  def log_eval_vars()
    eval("Chef::Log.info('locals:'+local_variables.join(':') + '\nglobals:'+global_variables.join(':'))")
  end

  def self.get_value_by_type(node, type)
    location = node[:nagios][type]
    e = Evaluator.new(node)
    val = e.eval_with_context(location)
    Chef::Log.debug("Looking at #{location} for #{type}. Got: #{val}")
    val
  end
  
end
end
