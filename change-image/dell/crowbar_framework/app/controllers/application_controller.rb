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

require 'uri'

# Filters added to this controller apply to all controllers in the application.
# Likewise, all the methods added will be available for all controllers.
class ApplicationController < ActionController::Base

  before_filter :digest_authenticate

  def digest_authenticate
    arr = $htdigest
    realm = arr[0].split(":")[1]

    return false if realm.nil?

    # Just return the crypted (hashed) version of the password if it's in the supported
    # format.  Note that the realm here "UserRealm" should match the middle
    # argument of your password hash
    authenticate_or_request_with_http_digest(realm) do |username|
      arr.each do |entry|
        list = entry.split(":")
        return list[2] if list[0] == username
      end
    end
  end

  # Basis for the reflection/help system.

  # First, a place to stash the help contents.  
  # Using a class_inheritable_accessor ensures that 
  # these contents are inherited by children, but can be 
  # overridden or appended to by child classes without messing up 
  # the contents we are building here.
  class_inheritable_accessor :help_contents
  self.help_contents = []

  # Class method for adding method-specific help/API information 
  # for each method we are going to expose to the CLI.
  # Since it is a class method, it will not be bothered by the Rails
  # trying to expose it to everything else, and we can call it to build
  # up our help contents at class creation time instead of instance creation
  # time, so there is minimal overhead.
  # Since we are just storing an arrray of singleton hashes, adding more
  # user-oriented stuff (descriptions, exmaples, etc.) should not be a problem.
  def self.add_help(method,args=[],http_method=[:get])
    # if we were passed multiple http_methods, build an entry for each.
    # This assumes that they all take the same parameters, if they do not
    # you should call add_help for each different set of parameters that the
    # method/http_method combo can take.
    http_method.each { |m|
      self.help_contents = self.help_contents.push({
                                           method => {
                                             "args" => args,
                                             "http_method" => m
                                           }
                                         })
    }
  end

  #helper :all # include all helpers, all the time

  protect_from_forgery # See ActionController::RequestForgeryProtection for details

  def self.set_layout(template = "application")
    layout proc { |controller| 
      if controller.is_ajax? 
        return nil
      end
      template
    }
  end
  
  def is_ajax?
    request.xhr?
  end
  
  add_help(:help)
  def help
    render :json => { self.controller_name => self.help_contents.collect { |m|
        res = {}
        m.each { |k,v|
          # sigh, we cannot resolve url_for at class definition time.
          # I suppose we have to do it at runtime.
          url=URI::unescape(url_for({ :action => k,
                        :controller => self.controller_name,
                        
                      }.merge(v["args"].inject({}) {|acc,x|
                                acc.merge({x.to_s => "(#{x.to_s})"})
                              }
                              )
                      ))
          res.merge!({ k.to_s => v.merge({"url" => url})})
        }
        res
      }
    }
  end
  set_layout
end
