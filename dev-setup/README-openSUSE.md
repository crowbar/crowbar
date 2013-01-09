# Crowbar Dev environment based on openSUSE

## Setting up the virtual machine (VM)

Currently only openSUSE 12.2 is supported, though the instructions here should
also work with other versions.

The steps here describe how to setup the VM from the command line. You can use
[virt-manager](http://virt-manager.org) if you prefer a graphical user
interface. Do submit your relevant virt-manager configs if you have some!

The steps here assume that your KVM host is also the desktop that you are
working from. If not, adapt the commands accordingly.

Installation steps:

1. Download the latest version of the Crowbar Dev VM image from [SUSE Gallery]
   (http://susestudio.com/a/n0rKOx/crowbar-dev). We recommend using the KVM
   image. Place the image in the `dev-setup/qemu-kvm` directory of the Crowbar
   git checkout on your KVM host.

1. [Optional] To improve VM performance, run the following example commands to
   pre-allocate the virtual disk:

   ````
   kvm-host> VERSION=1.0.5
   kvm-host> mv Crowbar_Dev.x86_64-$VERSION{,-org}.qcow2
   kvm-host> qemu-img convert -f qcow2 -O qcow2 -o preallocation=metadata Crowbar_Dev.x86_64-$VERSION{-org,}.qcow2
   ````

1. Start a VM with the desired network (private network with NAT), with the ISO
   and disk attached. For example:

   ````
   kvm-host> VERSION=1.0.5
   kvm-host> sudo qemu-kvm -m 2G -daemonize -vnc :10 \
                           -net nic,model=virtio,macaddr=DE:AD:BE:EF:30:22 \
                           -net tap,script=qemu-ifup \
                           -drive file=Crowbar_Dev.x86_64-$VERSION.qcow2,cache=none,if=virtio
   ````

   Note that `script=qemu-ifup` points to the script at `qemu-kvm/qemu-ifup`,
   so make sure you are running the above command in the same directory, or
   modify it accordingly.

1. [Optional] Connect to the VM via VNC. This is useful for debugging the VM
   (eg. networking issues).

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


1. After the VM boots up (takes a bit longer during the first boot), you should
   be able to connect to the VM via SSH:

   ````
   kvm-host> ssh root@192.168.124.10
   ````

   The root password is `linux`. If you cannot connect via SSH, this means that
   the networking is misconfigured. Refer to the previous step to see how you
   can connect via VNC to debug.

1. Create a non-root user account and set the passowrd. We recommend using the
   same username as you do on your regular workstation for convenience. Then
   re-login to the dev VM as the newly created user. For example:

   ````
   root@crowbar-dev> useradd -m jamestyj
   root@crowbar-dev> passwd jamestyj
   root@crowbar-dev> logout
   jamestyj@kvm-host> ssh 192.168.124.10
   jamestyj@crowbar-dev>
   ````


## Setting up the development environment

You should now have a working VM that you can SSH into from the qemu-kvm host.

1. Copy your .gitconfig and other configuration files to the VM, eg:

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
   for details. This will take a while so get some coffee.

1. Setup the test and development environment:

   ````
   crowbar-dev> ./dev setup-unit-tests --no-gem-cache
   crowbar-dev> cd /tmp/crowbar-dev-test && bundle install
   ````

   This setups a Crowbar run-time environment at
   `/tmp/crowbar-dev-test/crowbar_framework`.

1. You should now be able to run the unit and Rspec tests:

   ````
   cd /tmp/crowbar-dev-test/crowbar_framework
   bundle exec rake db:drop db:migrate db:fixtures:dump test:units spec
   ````

1. [Broken] Running the BDD tests:

   ````
   cd /tmp/crowbar-dev-test/crowbar_framework/BDD
   ./linux_compile.sh
   ./linux_run.sh
   ````

   Unfortunately the BDD tests (written in Erlang) currently do not work on
   openSUSE. We are working to resolve this. Meanwhile you can refer to the
   [BDD dev guide]
   (https://github.com/crowbar/barclamp-crowbar/blob/master/crowbar_framework/doc/default/crowbar/devguide/testing/bdd.md)
   for more details on how it works.

1. Starting the Crowbar web interface:

   ````
   cd /tmp/crowbar-dev-test/crowbar_framework
   bundle exec rails s puma
   ````

   You will want to keep this terminal open to see the Rails logs, which will
   come in very handy during development and debugging. The server can be
   terminated with `Ctrl-c`.

   The Crowbar web interface should now be accessible from your host web
   browser, eg. at `http://192.168.124.10:3000`.

Happy hacking! We will be updating this document regularly as we expand the
supported distros and evolve the code base. Pull requests are very much
appreciated!
