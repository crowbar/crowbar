

Welcome to the Crowbar project

The code and documentation is distributed under the Apache 2 license (http://www.apache.org/licenses/LICENSE-2.0.html). Contributions back to the source are encouraged.

7/26 Note: We are updating this information as fast as possible today. Please be patient!

Crowbar was developed by the Dell CloudEdge Solutions Team (http://dell.com/openstack) as a OpenStack installer (http://OpenStack.org) but has evolved as a much broader function tool. It is a wrapper for Opscode Chef Server (http://opscode.com)

Much of the design information about Crowbar has been published on Rob Hirschfeld's Blog (http://robhirschfeld.com).

This wiki maintains information about the operation of Crowbar. Documentation for users and deployment will be committed into the Git repo. Intermediate updates are likely to surface in the wiki.

Please consult the release notes for known issues

To build your own copy of Crowbar, consult the readme (https://github.com/dellcloudedge/crowbar/blob/master/README.build) in the crowbar repos. Some important notes about the build process: * Build requires a component known as "sledgehammer" that is a CentOS based image for discovery. This TFTPboot image does not change often and is not part of the normal build steps. See the DellCloudEdge/crowbar-sledgehammer repo for details. * Build process has been tested on Ubuntu 10.10 and CentOS

We are working to make this wiki complete, please feel free to update content.
