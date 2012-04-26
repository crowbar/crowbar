
Welcome to the Crowbar project

The code and documentation is distributed under the Apache 2 license (http://www.apache.org/licenses/LICENSE-2.0.html). Contributions back to the source are encouraged.

Crowbar was developed by the Dell CloudEdge Solutions Team (http://dell.com/openstack) as a OpenStack installer (http://OpenStack.org) but has evolved as a much broader function tool. It is a wrapper for Opscode Chef Server (http://opscode.com)

Much of the design information about Crowbar has been published on Rob Hirschfeld's Blog (http://robhirschfeld.com).

This wiki maintains information about the operation of Crowbar. Documentation for users and deployment will be committed into the Git repo. Intermediate updates are likely to surface in the wiki.

Please consult the release notes for known issues

To build your own copy of Crowbar, consult the readme (https://github.com/dellcloudedge/crowbar/blob/master/README.build) in the crowbar repos. Some important notes about the build process: 
* Build process has been tested on Ubuntu 10.10 and CentOS

With that said, from a fresh install off the crowbar-dev.iso, the next steps are:

0: Boot using the ISO and it will setup Ubuntu 10.10 and stage Crowbar for install
1: Log in as crowbar/crowbar (Ubuntu) or root/crowbar (Redhat)
2: sudo su - root
3: cd /tftpboot/ubuntu_dvd/extra (Ubuntu) or cd /tftpboot/redhat_dvd/extra (Redhat)
4: ./install admin.crowbar.org (or whatever FQDN you want your admin node to have).

Note: if the install did not work, you can try "sudo chef-client" and that may fix issues by repeating the last part of the installation.

The rest of Crowbar will then install.  Unless you change the networking defaults, you can access the Crowbar server:
* Crowbar UI on http://192.168.124.10:3000.  (crowbar/crowbar)
* Chef UI on http://192.168.124.10:4040.  (admin/password)
* Nagios on http://192.168.124.10/nagios3.  (nagiosadmin/password)
* Ganglia on http://192.168.124.10/ganglia.  (nagiosadmin/password)

We are working to make this wiki complete, please feel free to update content.

