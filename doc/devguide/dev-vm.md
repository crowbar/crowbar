# Getting started with Crowbar development

Setting up a full Crowbar development environment is complex due to its many
dependencies - we are simplifying and automating this process as much as
possible. This document provides detailed instructions on how to setup a
_minimal Crowbar development instance_: access to the web interface and the
ability to run the unit, RSpec, and BDD tests.

We assume you are setting up the Crowbar development environment in a qemu-kvm
virtual machine (VM). It is not a hard requirement - just adapt the steps and
commands accordingly.

If you prefer other hypervisors, check out the corresponding [VirtualBox]
(https://github.com/crowbar/crowbar/wiki/Running-Crowbar-in-VirtualBox-VMs) and
[VMWare]
(https://github.com/crowbar/crowbar/wiki/Running-Crowbar-in-VMWare-VMs) docs.
Then skip to the "Setting up the development environment" section of your
preferred distro.

If you are using Fedora 18, [these scripts]
(https://github.com/cwolferh/crowbar-virt-for-f18) may save you a bit of time
setting up a qemu-kvm/virsh environment for Crowbar.

## Setting up the qemu-kvm host

### Installing KVM

First you need to install KVM. On SUSE based systems, run:

    sudo zypper in kvm

### Enabling CPU virtualization acceleration

[Intel VT-x]
(http://en.wikipedia.org/wiki/X86_virtualization#Intel_virtualization_.28VT-x.29)
or [AMD-V]
(http://en.wikipedia.org/wiki/X86_virtualization#AMD_virtualization_.28AMD-V.29)
capable CPUs are required for hardware acceleration. This is usually disabled
by default in the BIOS, so you may need to enable it manually.

Run the [qemu-kvm/setup-kvm]
(https://github.com/crowbar/crowbar/blob/master/dev-setup/qemu-kvm/setup-kvm)
script to set it up. It checks for CPU support and loads the appropriate kernel
modules.

## Setting up the virtual machine

Refer to the following distro specific docs: [openSUSE / SLES](dev-vm-SUSE.md),
[Ubuntu](dev-vm-Ubuntu.md), and [Fedora](dev-vm-Fedora.md).
