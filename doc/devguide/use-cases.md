# Crowbar Use Cases

## Personas

| Name | Title | Notes |
|------|-------|-------|
| Oscar | Operations Chief | Knows of Chef or Puppet. Likely has some experience. Comfortable and likes Linux. Can work with network configuration, but does not own network. Has used VMware. |
| Charlie | CIO | Concerned about time to market and ROI. Is working on commercial offering based on OpenStack. |
| Denise | Cloud Developer | Working on adding features to OpenStack. Working on services to pair with OpenStack. Comfortable with Ruby code. |
| Quick | Data Center Worker | Can operate systems. Can Power On/Off when supervised by Susan. In charge of rack and replacement of gear. Can Intiate Automation when directed and supervised, but not create automation. |
| Sally | Support Center Worker | Person who takes calls about the installation. |
| Susan | System Administrator | Responsible for the OS installation, tuning, config, the works. Knows of Chef/Puppet but decided they were not there yet, so wrote her own. Responsible for the Application Installation, but not the tuning or more than the base config. Responsible for Network connectivity to some point, but works with Networking Ops to ensure right. |
| Albert | Security Analyst/Operations | Goal to prevent any Interaction with any Machine/OS/Application, unless the proper information/justification is supplied.|

## Use cases

### 1. Get equipment setup to base

Event: The equipment has just arrived.

* Quick checks the manifest to make sure that the equipment arrived.
* Quick racks the servers and switch following the wiring chart provided by Oscar
* Quick follows the installation guides BIOS and Raid configuration parameters for the Admin Node
* Quick powers up the servers to make sure all the lights blink then turns them back off
* Oscar arrives with his laptop and the crowbar ISO
* As per instructions, Oscar wires his laptop to the admin server and uses VMplayer to bootstrap the ISO image
* Oscar powers system and selects network boot (system may automatically do this out of the "box", but can reset if need be).
* Once the bootstrap and installation of the Ubuntu-based image is completed, Oscar disconnects his laptop from the Admin server and connects into the switch
* Oscar logs into the Admin node and configures base admin parameters:
  * Hostname
  * Networks (admin and public required): admin IPs, routers, masks, subnets, usable ranges (mostly for public).
  * Optional: ntp server(s)
  * Optional: forwarding nameserver(s)
  * Passwords and accounts
  * Manually edits files that get downloaded.
* Run the Install Command
* System validates configuration for syntax and obvious semantic issues.
* Oscar clears switch config and sets the configuration per guide.
* Oscar configures his laptop for DHCP to join the admin network.
* Oscar looks at the Chef UI and verifies that it is running and he can see the Admin node in the list.
  * The Install guide will describe this first step and initial passwords.
  * The install guide will have a page describing a valid visualization of the environment.
* Oscar powers on the next node in the system and monitors its progress in Chef.
  * The install guide will have a page describing this process.
  * The Crowbar status page will have the node arrive and can be monitored from there. Completion occurs when the node is "checked in". Intermediate states can be viewed by checking the nodes state attribute.
  * Node transitions through defined flow process for discovery, bios update, bios setting, and installation of base image.
  * crowbar_watch_status - for terminal status updates.
* Once Oscar sees the node report into Crowbar, Oscar shows Quick how to check the system status and tells him to turn on the rest of the nodes and monitor them.
* Quick monitors the nodes while they install. He calls Oscar when they are all in the “ready” state. Then he calls Oscar back.
* Quick Verify BMC/IPMI/SSH availability
* Oscar checks their health in Nagios and Ganglia (Report for Machine information).
  * If there are any red warnings, Oscar works to fix them.

### 2. Install OpenStack Swift

Event: System checked out healthy from base configuration

* Oscar logs into the Crowbar portal
* Oscar selects swift role from role list
* Oscar is presented with a current view of the swift deployment (Which starts empty).
* Oscar asks for a proposal of swift layout
  * The UI returns a list of storage, auth, proxy, and options.
* Oscar may take the following actions:
  * He may tweak attributes to better set deployment (Networking options...)
  * He may force a node out or into a sub-role
  * He may re-generate proposal
  * He should verify the propsal
  * He will edit/import the new proposal
  * He may commit proposal
* Oscar may validate progress by watching:
  * Crowbar main screen to see that configuration has been updated.
  * Nagios to validate that services have started
  * Chef UI to see raw data..
* Oscar checks the swift status page to validate that the swift validation tests have completed successfully.
* If Nagios Swift Status reports failure, Oscar uses troubleshooting guide to correct problems or calls support.
  * Swift is installed correctly
  * Nagios reports it is installed
  * Swift Installed Validation
* Oscar re-verify the state that Nagios is reporting, to see if corrective action worked.
* Oscar is directed to Swift On-line documentation for using a swift cloud from the install guide

### 3. Install OpenStack Nova

Event: System checked out healthy from base configuration

* Oscar logs into the Crowbar portal
* Oscar selects nova role from role list
* Oscar is presented with a current view of the nova deployment (Which starts empty)
* Oscar asks for a proposal of nova layout
  * The UI returns a list of options, and current sub-role usage (6 or 7 roles).
  * If Oscar has already configured swift, the system will automatically configure glance to use swift.
* Oscar may take the following actions:
  * He may tweak attributes to better set deployment (Use admin node in nova, Networking options...)
  * He may force a node out or into a sub-role
  * He may re-generate proposal
  * He may commit proposal
* Oscar finishes configuration proposal and commits proposal.
* Oscar may validate progress by watching:
  * Crowbar main screen to see that configuration has been updated.
  * Nagios to validate that services have started
  * Chef UI to see raw data
* Oscar checks the nova status page to validate that the nova validation tests have completed successfully.
* If nova validation tests fail, Oscar uses troubleshooting guide to correct problems or calls support.
  * Oscar uses re-run validation test button to see if corrective action worked.
* Oscar is directed to Nova On-line documentation for using a nova cloud from the install guide.

### 4. Pilot and Beyond Use Cases

#### 4a. Unattended refresh of system

This is a special case, for Denise.

* Denise is making daily changes to OpenStack’s code base and needed to test it. She has committed changes to their git code repository and started the automated build process
* The system automatically receives that latest code and copies it to the admin server
* A job on admin server sees there is new code resets all the work nodes to “uninstalled” and reboots them.
* Crowbar reimages and reinstalls the images based on its cookbooks
* Crowbar executes the test suites against OpenStack when the install completes
* Denise reviews the test suite report in the morning.

#### 4b. Integrate into existing management

Event: System has passed lab inspection, is about to be connected into the corporate network (or hosting data center)

* Charlie calls Oscar to find out when PoC will start moving into production
* Oscar realizes that he must change from Nagios to BMC on all the nodes or they will be black listed on the network.
* Oscar realizes that he needs to update the SSH certificates on the nodes so they can be access via remote. He also has to change the accounts that have root access.
* Option 1: Reinstall.
  * Oscar updates the Chef recipes to remove Nagios and add BMC, copy the cert and configure the accounts.
  * Oscar sets all the nodes to “uninstalled” and reimages the system.
  * Repeat above step until system is configured correctly
* Option 2: Update Recipes
  * Oscar updates the Chef recipes to remove Nagios and add BMC, copy the cert and configure the accounts.
  * Oscar runs the Chef scripts and inspects one of the nodes to see if the changes were made
