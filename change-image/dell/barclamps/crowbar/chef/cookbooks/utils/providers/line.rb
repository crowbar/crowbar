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

action :add do
  filename = "/tmp/#{new_resource.file.gsub("/","_")}.lock"
  f = ::File.new(filename, ::File::RDWR|::File::CREAT, 0644)
  rc = false
  count = 0
  while rc == false do
    count = count + 1
    rc = f.flock(::File::LOCK_EX|::File::LOCK_NB)
    sleep 1 if rc == false
  end

  system "/bin/egrep -q '^#{new_resource.name}$' '#{new_resource.file}'"
  ret=$?
  if ret != 0
    b = bash "add #{new_resource.name}" do
      code "/bin/echo '#{new_resource.name}' >> '#{new_resource.file}'"
    end
    b.action(:run)
    new_resource.updated_by_last_action(true)
  end

  f.flock(::File::LOCK_UN)
  f.close
end

action :remove do
  filename = "/tmp/#{new_resource.file.gsub("/","_")}.lock"
  f = ::File.new(filename, ::File::RDWR|::File::CREAT, 0644)
  rc = false
  count = 0
  while rc == false do
    count = count + 1
    rc = f.flock(::File::LOCK_EX|::File::LOCK_NB)
    sleep 1 if rc == false
  end

  system "/bin/egrep -q '^#{new_resource.name}$' '#{new_resource.file}'"
  ret=$?
  if ret == 0
    line_str = new_resource.name.gsub("\"", "\\\"").gsub("/", "\\/").gsub("'", "\\'")
    b = bash "remove #{new_resource.name}" do
      code "/usr/bin/perl -ni -e 'print unless /^#{line_str}$/' '#{new_resource.file}'"
    end
    b.action(:run)
    new_resource.updated_by_last_action(true)
  end

  f.flock(::File::LOCK_UN)
  f.close
end

