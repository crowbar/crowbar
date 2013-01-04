# Getting started with Crowbar development

Setting up a full Crowbar development environment is rather complex due to its
many dependencies. We are working on simplifying and automating this process as
much as possible. This document provides step by step instructions on how to
setup a _minimal Crowbar development instance_ - access to the web interface
and the ability to run the unit, RSpec and BDD tests.

Here we assume that you are setting up the Crowbar development
instance/environment in a qemu-kvm virtual machine (VM), with a SUSE based
system as the VM host. There is no hard requirement for this - you just need to
adapt the steps and commands listed here accordingly.

If you prefer to use another hypervisor other than qemu-kvm, see [Crowbar in
VirtualBox](https://github.com/crowbar/crowbar/wiki/Running-Crowbar-in-VirtualBox-VMs)
or [Crowbar in VMWare](https://github.com/crowbar/crowbar/wiki/Running-Crowbar-in-VMWare-VMs).
Then jump to the section titled "Installing the development system" and adapt
the steps/commands accordingly.

## Setting up the qemu-kvm host

### Installing KVM

First you need to install KVM. On SUSE based systems, run:

    sudo zypper in kvm

### Enabling CPU virtualization acceleration

qemu-kvm requires hardware support for optimal performance. This requires
[Intel VT-x](http://en.wikipedia.org/wiki/X86_virtualization#Intel_virtualization_.28VT-x.29)
or [AMD-V](http://en.wikipedia.org/wiki/X86_virtualization#AMD_virtualization_.28AMD-V.29)
capable CPUs. This is usually disabled by default in the BIOS, so you may need
to enable it manually.

You can run the [qemu-kvm/setup-kvm](https://github.com/crowbar/crowbar/blob/master/dev-setup/qemu-kvm/setup-kvm)
script to set it up. It will check for CPU support and load the appropriate
kernel modules.

## Setting up the virtual machine

Currently two Linux distributions are supported as development enviromnents,
though others should also work:

1. openSUSE - Refer to the [openSUSE setup steps](https://github.com/crowbar/crowbar/blob/master/dev-setup/README-openSUSE.md) for
   detailed instructions on how to proceed

1. Ubuntu - Refer to the [Ubuntu setup steps](https://github.com/crowbar/crowbar/blob/master/dev-setup/README-Ubuntu.md) if you prefer
   using Ubuntu.
