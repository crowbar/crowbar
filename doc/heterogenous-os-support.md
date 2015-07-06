This file documents how to deploy multiple different operating systems
in Crowbar.

Right now deploying a hetrogenous OS cluster in Crowbar relies on a
few prerequisites:
 * Online mode.
   You need to have the provisioner configure to operate in online
   mode to deploy multiple different operating systems.  This is
   needed to allow the operating systems to pick up all the packages
   they well need for a node installation. See README.package-updates
   for more information about setting the provisioner to online-mode.
 * online_mirror stanzas in the deployed provisioner proposal for all
   operating systems you wish to deploy.
   These are needed so that the provisioner find the proper location
   to copy all the information needed to kick off a network-based
   installation of an operating system.  The defaout proposal includes
   stanzas for the Ubuntu and CentOS operating system releases Crowbar
   supports.
 * Proper recipe supoprt for the OS in the provisioner.
   Right now we only include support for Ubuntu and CentOS.  Adding
   Redhat support is trivial, and adding SuSE support is slightly less
   so.

Once the above prerequisites are met, you can use the crowbar
provisioner command to query the list of available oses a node can
install, and view and change the OS the provisioner will deploy on to
a node.  The new commands are:
 * crowbar provisioner oses
 * crowbar provisioner current_os <nodename>
 * crowbar provisioner set_os <nodename> <os>

You can run these commands at any time, and the next time a node
transitions through the installing or reinstall states it will have
the appropriate operating system installed.
