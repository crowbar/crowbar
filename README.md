# Welcome to the Crowbar project [![Build Status](https://travis-ci.org/crowbar/travis-ci-crowbar_framework.png?branch=master)](https://travis-ci.org/crowbar/travis-ci-crowbar_framework)

The code and documentation are distributed under the [Apache 2
license](http://www.apache.org/licenses/LICENSE-2.0.html). Contributions back
to the source are encouraged.

Crowbar was developed by the [Dell CloudEdge Solutions Team]
(http://dell.com/openstack) as an [OpenStack](http://OpenStack.org) installer,
but has evolved as a much broader function tool. It is a wrapper for Opscode
[Chef](http://opscode.com) Server.

Much of the design information about Crowbar has been published on Rob
Hirschfeld's [blog](http://robhirschfeld.com). This wiki maintains information
about the operation of Crowbar. Documentation for users and deployment will be
committed into the Git repo. Intermediate updates are likely to surface in the
wiki.

Please consult the release notes for known issues.

To build your own copy of Crowbar, consult the [readme]
(https://github.com/crowbar/crowbar/blob/master/README.build). Some important
notes about the build process:

* Build process has been tested on Ubuntu 12.04 and CentOS.

With that said, from a fresh install off the crowbar-dev.iso, the next steps
are:

1. Boot using the ISO and it will setup Ubuntu 12.04 and stage Crowbar for
   install.
1. Log in as `crowbar/crowbar` (Ubuntu) or `root/crowbar` (Redhat).
1. `sudo su - root`.
1. `cd /tftpboot/ubuntu_dvd/extra` (Ubuntu) or `cd /tftpboot/redhat_dvd/extra`
   (Redhat).
1. `./install admin.crowbar.org` (or whatever FQDN you want your admin node to
   have).

Note: If the install did not work, you can try `sudo chef-client` and that may
fix issues by repeating the last part of the installation.

The rest of Crowbar will then install. Unless you change the networking
defaults, you can access the Crowbar server:

* Crowbar UI on http://192.168.124.10:3000.  (crowbar/crowbar)
* Chef UI on http://192.168.124.10:4040.  (admin/password)
* Nagios on http://192.168.124.10/nagios3.  (nagiosadmin/password)
* Ganglia on http://192.168.124.10/ganglia.  (nagiosadmin/password)

We are working to make this wiki complete, please feel free to update content.
