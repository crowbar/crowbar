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
class RemoteServiceObject
  require 'patron'

  def self.build_session(timeout) 
    sess = Patron::Session.new
    sess.timeout = timeout
    sess.headers['Accept'] = "application/json"
    sess.headers['Content-Type'] = "application/json"
    sess
  end

  def self.common_return(res)
    return [res.body, res.status.to_i ] if res.status.to_i != 200
    struct = JSON.parse(res.body)
    [struct, 200]
  end

  # Helper routines to contact the service in question.
  def self.post_json(path, data, timeout = 120)
    sess = build_session timeout
    res = sess.post(path, data)
    common_return res
  end

  def self.put_json(path, data, timeout = 120)
    sess = build_session timeout
    res = sess.put(path, data)
    common_return res
  end

  def self.get_json(path, timeout = 120)
    sess = build_session timeout
    res = sess.get(path)
    common_return res
  end
end

