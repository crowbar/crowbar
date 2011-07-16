DESCRIPTION
===========
Chef Cookbook to install and configure Glance API and Registry.

REQUIREMENTS
============
Requires access to Glance packages.

Recipes
=======
api
---
common and service

common
------
package, directories, template

registry
--------
common and service

setup
-----
includes common, adds tty linux

TODO
====
- rewrite setup.rb to use Chef Resources(merge with common?)
- move away from hard-coded tty image to an array of downloads
- can glance use the mysql for sqlconnection from nova(does it even matter?)

License
=======
Author:: Dan Prince <dan.prince@rackspace.com>

Author:: Matt Ray <matt@opscode.com>

Copyright:: 2011 Opscode, Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
