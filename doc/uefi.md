This file documents the current state of UEFI booting in Crowbar

Right now, we have basic support for network booting systems running
in UEFI mode. We are able to netboot Sledgehammer and the OS install
kernel/initrd pairs, and get to the point where we have an installed
operating system on the compute nodes.

How To Use UEFI:
 * Switch your system to operate in UEFI mode.  How this is done
   varies from system to system.
 * Once the system is in UEFI mode, configure it to network boot off
   the first nic.
 * The crowbar framework will handle things from there.

What UEFI Gives You:
 * Native support for drive sizes > 2TB.
 * All new low-level system firmware.

What Does Not Work:
 * Booting Ubuntu 12.04 off the hard drive.

   There is a bug in how the version of Grub 2 that comes with Ubuntu
   handles memory mapping in the UEFI environment that causes the
   system to crash the UEFI firmware when it tries to load the kernel
   and initrd.  Upstream grub2 has been patched to resolve this issue,
   but the updates have not been pulled into Ubuntu 12.04 yet.

   Patch is at http://savannah.gnu.org/bugs/?36532.

   Possible workarounds include:
   * Look at using grub-legacy or elilo instead of grub2 when
     installing in UEFI mode.
   * Working on getting Canonical to pull in an updated version of
     Grub2 that has the patch that fixes the issue.

What Had to Be Changed:

 * We now generate per-node OS installation scripts and UEFI/PXE
   config files on the fly instead of having per-OS install scripts
   and config files.  This was needed to support network installation
   of Redhat and CentOS due to anaconda not being able to find out
   what NIC it booted from when installing in UEFI mode.  For
   consistency between the UEFI and PXE netboot codepaths, the
   per-machine UEFI and PXE config files are now IP address specific
   instead of MAC address specific.

 * When systems operate in UEFI mode, we manage the boot order
   directly instead of assuming that we will always netboot.

   Each of the OSes we support uses a different bootloader for UEFI
   (CentOS and Redhat use grub1, and Ubuntu uses grub2), and they do
   not have the right intersection of being able to netboot, being
   able to chainload, and operating reliably in a network environment
   to be useful.  Instead we use elilo, and we grab the prcompiled EFI
   apps from Sourceforge instead of trying to compile things ourselves,
   playing packaging shennanigans to keep our admin node bootable
   while juggling multiple bootloaders, or have the provisioner handle
   messing with boot templates for multiple different bootloaders.

   However, none of the UEFI capable bootloaders I have found have the
   ability just hand control back to UEFI to try the next thing in the
   boot sequence, which means that we can no longer assume that we
   will always netboot. Instead, I have added efibootmgr to
   Sledgehammer and the default package installs for the operating
   systems, and added a piece of code to the crowbar-hacks recipe
   in the deployer that knows how to change the boot order to either
   be nics-first or nics-last, and we set that based on the PXE state
   machine -- if it is in execute state, we will set things to
   nics-last, otherwise it will set things to nics-first.

   In order to make that work reliably, I had to modify the
   provisioner state machine to run chef-client on every node that is
   transitioning out of the execute state in order to ensure that the
   boot sequence gets updated.  This failed when transitioning into
   reset or delete, because those states deallocate all IP addresses
   to the nodes.  To cope with that, the network barclamp recognizes
   when the admin network is being removed and arranges for the system
   to run dhclient on all the interfaces until it gets an IP address.
   Since we run chef-client on the nodes before running it on the
   admin node, this should result in the system getting the same IP
   address it had before, which will let the chef-client run continue.
