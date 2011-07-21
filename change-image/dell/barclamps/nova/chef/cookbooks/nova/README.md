Description
===========
Cookbook and recipes for deploying OpenStack Nova.

Requirements
============
Written and tested with Ubuntu 10.04 and 10.10 and Chef 0.10.0. 

Definitions
===========
nova_package
------------
This handles installing nova packages generically and managing them as services.

Resources/Providers
===================

Recipes
=======
api
---
Installs the `nova-api` service.

config
------
Shared nova dependencies and writes out the `nova.conf` file.

compute
-------
Installs the `nova-compute` service and currently depends on KVM.

dashboard
---------
Untested.

mysql
-----
Installs and configures MySQL for use by Nova.

network
-------
Installs the `nova-network` service.

objectstore
-----------
Installs the `nova-objectstore` service.

project
-------
Configures the Nova project, network and permissions.

rabbit
------
Installs and configures RabbitMQ for use by Nova.

scheduler
---------
Installs the `nova-scheduler` service.

volume
------
Untested.

License
=======
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
