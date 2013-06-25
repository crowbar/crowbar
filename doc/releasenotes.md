# Release Notes

Tracking the release state information for Crowbar 

## What's New

### Version 1.4

### Version 1.3

This section is provided as a shortcut for previous Crowbar users.
•	Node Alias and Grouping feature in Dashboard
•	Node Alias & Description population in Bulk Edit
•	Networking Menu: Switch & VLAN views
•	Utility Menu: Export Logs & Chef

## Release Notes

### Crowbar 1.2 Release Notes (December 19, 2011)

| **Component** | **Title** | **Comments** | **Work Around** | **Fixed In** | **Ref#** |
| Crowbar UI | IE7: Attribute edit textbox translates apostrophe into web safe ' | Only on some browsers | Upgrade to IE8 (or turn off IE7 compatablity) or Change to \" to correct | Not Fixed | 143 |
| Crowbar UI | Rebooting a machine before Allocation causes endless looping or booting into old code | Cannot use IPMI buttons before system is ready. | User should verify "ready" state on node before using. Note: Buttons are no longer visible until Node has been allocated. | Not Fixed | 97, 150 |
| Crowbar UI | Connecting with HTTPS produces bad SSL message error | HTTPS was not a target for the 1.0 release | Reference the Crowbar UI by http:// not https:// | Not Fixed | 67, 162 |
| Crowbar UI | Adding/Removing a role in Chef does not update the proposal. | If Crowbar does not do the updating then it cannot be reflected in the proposal! | Make changes for proposals in Crowbar | Not Fixed | 87 |
| Crowbar UI | There is no way to completely delete a node | Crowbar will remove many aspects of a node from the system, but does not delete all traces (such as IP allocation). Can result in lost IP allocations. | No work around; however, it will be possible to identify lost IPs and recover. | Not Fixed | 166 |
| Crowbar UI | HW Update button shouldn't set RAID config | Crowbar will apply ALL pending BIOS and RAID changes if the "HW Update" button is selected. Changing the RAID will LOSE ANY DATA on the driver array! BIOS/RAID changes require a reboot. | Don't change the RAID configuration after the node is deployed. Backups are nice too. | Not Fixed | 168 |
| Crowbar UI | Crowbar validation not working  | Submitted a proposal with a node listed twice as slave node in Hadoop| Do not enter the same node twice in any proposal. | Not Fixed | 284 |
| Crowbar UI | Drag and drop is not enabled in IE8  | | Use “Raw” mode or see the User’s Guide for a list of supported browsers | Not Fixed | 617 |
| Crowbar UI | Bulk Edit selection not sticking for Storage  | When using Bulk Edit to set multiple machines Raid/BIOS settings, on a node where BIOS=Storage and RAID=JBOD was selected and Saved, it didn't stick.| Workaround: Use Node Edit. | Not Fixed | 621 |
| Crowbar UI | Missing BMC Buttons after shutdown of node using "Power Off Button"  | After clicking Power Off button on a node, the other buttons under Node – Edit were missing.| Run the following command:  ipmitool -H BMCIP -P crowbar -U crowbar power on | Not Fixed | 196 |
| Crowbar UI | No logout from crowbar | | Close open browser windows to remove temporary cookies. | Not Fixed | 242 |
| Crowbar UI | Nodes in “Ready Retrying” that are powered off stay flashing red in UI  | I had two nodes in ready retrying state and shut them down.  They stayed flashing “Ready Retrying” and not grey, “Unavailable” | This indicates a failure to properly initialize the node. Troubleshoot the underlying problem by inspecting logs and then reinstall the node.  If the node failure is permanent, delete the node from crowbar. | Not Fixed | 407 |
| Glance | Not logging the Glance commit to Admin node error.log or syslog | This will be addressed in the Diablo release. Please review the Glance barclamp documentation for more information. | Until then, look for =glance-registry.log= and =glance-api.log= in =/var/log/glance= on the glance server. | Not Fixed | 104, 85 |
| Keystone | Modifying account info on an active proposal and reapplying does not work  | | User must make Keystone account changes before applying proposal. | Not Fixed | 581 |
| Install | Admin node BIOS, RAID, and customization requires Keyboard/Monitor to be available | Work to apply Crowbar to Crowbar is planned, but did not make it into the initial the release. | Get a KVM for admin node setup. Refer to "Getting Started" guide for specific settings. | Not Fixed | 102 |
| Install | Network configuration cannot be changed after Admin node is setup. Users must make any network barclamp changes PRE-Admin install | Bootstrapping issue (related to #67). Expected to add it to setup process.  | Before running "install," edit network barclamp file in <br />=/opt/dell/chef/data_bags/crowbar/bc-template-network.json=. For details, please refer to the Networking Barclamp documentation. | Not Fixed | 109, 97, 10 |
| Install | We need to tell users how to configure BMC for Admin node | The default configuration is to use a dedicated static IP set to 192.168.124.162 | If you have changed the default networking (109 above) then you must update the Admin node BMC to match. The IP range varies depending on your configuration. | Not Fixed | 90 |
| Install | Booted machine on network with &gt;1 installed Crowbar creates conflicts between servers. | Crowbar is a DHCP server, you cannot have two of them on the same network | When installing a new Crowbar server, replace the old one instead of bringing it up on a different server. | Not Fixed | 151 |
| Install | Crowbar only supports 1 top level domain | While it is possible to change the TLD, Crowbar UI assumes that there is only 1 TLD in numerous places.  | Only use 1 top level domain | Not Fixed | - |
| Install | BMCs may have conflicting IP addresses | When re-installing a cloud moving the Admin node from one system to another can cause conflicting IP addresses on the BMCs | If reinstalling a cluster use the same node as the Admin system or ensure that the BMCs are cleared of IP information | Not Fixed | 193 |
| Install | System can hang in transitioning from Allocating mode | In some cases the services a node might not shut down correctly when transitioning from the Allocating state to the Readying state | Reboot the node in question. It will correctly continue the installation process | Not Fixed | 201 |
| Install | Changing RAID on a node which already had a RAID set configured will intermittently give an error. | The OS installation will perodically generate an error "Volume group name already in use"  | Reboot the node exibiting the error. Alternatively a System Administrator can remove the existing VLM information from the disk sub-system. | Not Fixed | 183, 498 |
| Install | Changing the bonding mode requires a reboot | After changing the bonding I found that I had to reboot my nodes to get the new bond type to take. | If changing the bonding mode, reboot your nodes. | Not Fixed | 474 |
| Install | The network barclamp dosesn't clean up vlan interface on node when switching from single to dual | Deploy a swift storage node with network barclamp configured in single mode.  Edit network barclamp proposal to switch to dual mode, and save & apply proposal. On a compute node, inspect ifconfig and notice that there are both: eth0.200 and eth1.200 | Once the proposal has fully deployed, reboot the node to make the current active network configuration match the correctly created configuration files. | Not Fixed | 476 |
| Install | "Network autoconfiguration failed" error | Seen on one node after deploying Nova proposal | This error is caused by a slow switch. Make sure that switch ports are configured for quick STP convergence (the timeout allowed is 120 seconds) | Not Fixed | 499 |
| Install | BIOS not updating from some versions | Certain Versions cannot be updated to our current rev. 1.30 | Please see the BIOS Firmware release notes to determine if you need to disable the BIOS Barclamp prior to updating any machines.  | Not Fixed | 561 |
| Install | Modifying APT sources.list can break admin node | Customers might modify the system configuration to add additional packages into crowbar.   This could cause chef-client execution to fail on the admin node (Which currently occurs on a regular basis), and render the admin node non-functional. | It is highly recommended not to modify resources managed by crowbar.  If new packages are required, build a custom crowbar ISO and include the required packages in the build.  | Not Fixed | 463 |
| Install | Nodes that are not managed by the admin are permitted to register with Chef | I booted up the VM's and did not reset the boot sequence for network.  The nodes registered with Chef and admin.  Now I get an Error 500 page and see the nodes logging to the admin node their replication of swift. | Inspect the chef UI to identify the offending nodes, and bring them onto crowbar (PXE boot to install standard image).  If the nodes are not to be managed by crowbar, either stop the chef-client daemon, or point it to the appropriate chef-server.  | Not Fixed | 527 |
| Install | Long machine names cause failures | Long machine names cause failures related to chef-search| Use reasonably short domain names (10 characters or less). If already installed, see here: https://github.com/crowbar/crowbar/wiki/Crowbar-issues-and-workarounds---deails   | Not Fixed | 626 |
| Swift | Swift does not always start after inital proposal commit. Memcachd is not starting up | Occurs occasionally after the system is first installed. This only occurs during inital instal. | Reboot the Swift Proxy node. The installation will continue sucessfully | Not Fixed | 24 |
| Swift | Swift with multiple proxies breaks | Currently not supported by Swift recipes in Crowbar | Utilize single proxy | Not Fixed | 158 |
| Swift | Not all swift ring parameters updated | Some are: # of replicas, # of zones.  While some should never been updated (e.g. cluster hash) all ring parameters should allow dynamic modifications.  | Note that manual changes to the ring do take effect. | Not Fixed | 622 |
| Nova | Nova failed to install with specific config | Network proposal -> Nova fixed = 192.168.123.0 as a subnet and 255.255.255.0 as a mask.  Nova proposal -> num_networks =2 and network_size = 256  | The size of nova_fixed in the network proposal must be the same size as the nova proposal (num * size). | Not Fixed | 482 |
| Nova | euca2ools not installed by default on fresh nova deploy | I had to manually install euca2ools to try to query and manipulate images and VMs via the command line. | Use alternative methods to achieve the same capabilities: Dashboard,  Nova-manage commands | Not Fixed | 518 |
| Nova | IP address reported by knife search node 'roles:nova-multi-controller' is not constant | After running the command at different the IP address of the node that knife is reporting has changed.  I would expect it to remain the same. | Chef manages the ipAddress node information as it sees fit.  To get consistent results, use the node[crowbar][network] information for each one of the networks the node has interfaces on.  | Not Fixed | 516 |
| Nova | rabbitmq is periodically restarted | API calls occasionally fail, because rabbit gets restarted. | see here for workaround https://github.com/crowbar/crowbar/wiki/Crowbar-issues-and-workarounds---deails  | Fixed post v1.2 | 635|
| Nova Dashboard | Somtimes get MySQL connect error on dashboard | OperationalError) (2006, 'MySQL server has gone away' | It will clear if you refresh the page a couple of times. | Not Fixed | 487 |




