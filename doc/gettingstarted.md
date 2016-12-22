# Getting Started with Dell Crowbar

This book is designed as a quick start for new Crowbar users

## What is Crowbar?

Crowbar is an open source cloud deployment framework originally developed by Dell to support our OpenStack and Hadoop powered solutions.  Recently, it’s scope has increased to include a DevOps operations model and other deployments for additional cloud applications.

### Crowbar 1.0

Crowbar 1.0 was a wrapper around Chef, written in Ruby on Rails 2.x  It stored all of its information in Chef’s database as data_bags, roles and node objects.

The Crowbar Admin ISO contained an OS with all the required services and packages to deploy an OpenStack Nova and/or Swift cluster and their dependencies.  It allowed for deployment on just about any commodity hardware, but was certified on particular configurations of Dell hardware.

“Barclamps” are the modular units of code and configuration to deploy an entire cluster, or portions of a cluster.  One deploys those Barclamps onto nodes via “Proposals” (now called Barclamp Configurations.)  There are certain “default” barclamps (networking, dns, ntp, etc), barclamps that are portions of a cluster (mysql, glance, swift-storage) and there are barclamps that represent a whole cluster (nova).

Nodes are managed by the Crowbar state machine.  They are discovered via dhcp, hardware is discovered and configured via a special OS image that is installed temporariliy which we call “sledgehammer” and is activated by deploying barclamp configurations on them.  They can be decommissioned too.

> See Rob Hirschfeld's Background Post: http://robhirschfeld.com/2011/10/18/dell-crowbar-project-open-source-cloud-deployer/

### OpenCrowbar 2.0

OpenCrowbar 2.0 (and later renamed further) is a fork of Crowbar that changes the Crowbar
architecture in pretty fundamental ways.

### Crowbar 3.0

Crowbar 3.0 is an incremental improvement over Crowbar 1.0 that enables support for OpenStack
Liberty and is the core driver behind https://www.suse.com/documentation/suse-openstack-cloud-6/

### Crowbar 4.0

Crowbar 4.0 is an incremental improvement over Crowbar 3.0 that enables support for OpenStack
Newton and is the core driver behind SUSE OpenStack Cloud 7.



