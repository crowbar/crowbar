# Crowbar Dev based on openSUSE

Here we describe how to setup a Crowbar development environment in a virtual
machine (VM) that is based on openSUSE. It is currently focused on the core
Rails application and required barclamps.

## Setting up the VM

We assume that you are using KVM and have read the [KVM setup instructions]
(dev-vm.md). If not, setup the VM accordingly and continue on to the [next
section](#setting-up-the-development-environment).

The instructions here are command line only. If you prefer a GUI, try
[virt-manager](http://virt-manager.org) and please share your configs with us.
We assume your KVM host is the desktop you are working from, so adapt them if
necessary:

1. Download the latest version of the [Crowbar Dev VM image]
   (http://susestudio.com/a/n0rKOx/crowbar-dev) - KVM image recommended.
   Place the image in the `dev-setup/qemu-kvm` directory of the [Crowbar git]
   (https://github.com/crowbar/crowbar/) checkout on your KVM host.

1. [Optional] To improve VM disk performance, pre-allocate the virtual disk
   metadata:
   ````
   kvm-host> VERSION=2.1.0
   kvm-host> mv Crowbar_Dev.x86_64-$VERSION{,-org}.qcow2
   kvm-host> qemu-img convert -f qcow2 -O qcow2 -o preallocation=metadata Crowbar_Dev.x86_64-$VERSION{-org,}.qcow2
   ````

1. Start the VM by running:
   ````
   kvm-host> ./start-vm
   ````

1. After the VM boots up (takes a bit longer for first boot), you should be
   able to connect to the VM via SSH with root password `linux`:
   ````
   kvm-host> ssh root@192.168.124.10
   ````

1. Create a non-root user account and set the password. Use the same username
   as you do on your regular workstation for convenience. Then re-login to the
   dev VM as the newly created user, eg:
   ````
   root@crowbar-dev> useradd -m jamestyj
   root@crowbar-dev> passwd jamestyj
   root@crowbar-dev> logout
   jamestyj@kvm-host> ssh 192.168.124.10
   jamestyj@crowbar-dev>
   ````

### Troubleshooting tips

1. Connect to the VM via VNC. This is useful for debugging the VM (eg.
   networking issues).
   ````
   kvm-host> vncviewer :10
   ````

   The VM is configured with the following settings:

   ````
   IP address: 192.168.124.10
   Netmask:    255.255.255.0
   Gateway:    192.168.124.1
   DNS:        10.120.2.88, 8.8.8.8
   ````

   You may need to update the DNS setting to match your environment by
   modifying `/etc/resolv.conf`.


## Setting up the development environment

You should now have a working VM that you can SSH into from the qemu-kvm host.

1. Copy your .gitconfig and other configuration files to the VM, e.g.:
   ````
   crowbar-dev> scp -r <your-usual-dev-host>:.{gitconfig,vimrc,vim,profile,ssh} .
   ````

1. Check out the Crowbar git repo and run the dev tool:
   ````
   crowbar-dev> git clone git://github.com/crowbar/crowbar.git
   crowbar-dev> cd crowbar
   crowbar-dev> ./dev setup
   ````
   The `./dev setup` script will ask for your Github username and password. It
   will fork the Crowbar and corresponding barclamp repositories to your
   account and clone them into `crowbar/barclamps/`. See [dev-and-workflow]
   (https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow)
   and [dev-and-code-review]
   (https://github.com/crowbar/crowbar/blob/master/README.dev-and-code-review)
   for details.

Now see the [testing page](testing.md) for how to run the tests.
