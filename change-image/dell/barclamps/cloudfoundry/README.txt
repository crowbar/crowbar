8/24/2011 Keith Hudgins - keith@dtosolutions.com

This barclamp is a translation of the Cloud Foundry chef-solo recipes provided by VMWare that are used by their development-environment installer. I've had to do quite a bit of work to get the cookbooks running both in a chef-client environment and to slide through the crowbar server as it's prepping transitions for your client nodes.

What you get:

Apply this barclamp to an empty node and you'll get a Cloud Foundry development box. Log in as the openstack user and you'll have the CF install in ~/.cloudfoundry.

To start the environment, just do this:

cd ./cloudfoundry/vcap/devbox/vcap/dev_setup/bin
./vcap_dev start

You may have to run this twice, the cloud_controller rails app doesn't always start the first time it's run.

Once that's going, you can deploy your applications into the environment using vmc push. Just follow along with the VMWare documentation at http://cloudfoundry.org.

Limitations and known issues:

This is a development environment built against (at the time I'm writing this) 3-week old installer on a fast-moving product. Don't expect this (yet) to have the latest and greatest stuff from Cloud Foundry. If you need something added in here, dive in and contribute!

* Currently, this isn't packaged up neatly for install directly inside a Crowbar iso. You'll have to install the barclamp manually inside the iso. The instructions to do so are here:
http://kb.dtosolutions.com/wiki/Deploying_the_cloudfoundry_barclamp

* The cookbooks take two passes to converge properly. You can wait until chef-client runs a second time or speed this up by logging into the instance once it's built, and running 'sudo chef-client' manually.

* Node.js is installed in a strange place. Seems to work, though. (Please test!)

* The barclamp isn't configurable. I'm using the example templates for the UI, which are an UGLY yellow and have a useless option. Please ignore.

Questions should probably be directed to the crowbar listserv, which you can join at:
https://lists.us.dell.com/mailman/listinfo/crowbar

or email me at keith@dtosolutions.com