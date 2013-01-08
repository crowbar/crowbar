# Crowbar Dev environment based on Ubuntu

## Setting up the virtual machine (VM)

Currently only Ubuntu 12.04 LTS is supported, though the instructions here
should also work with other versions.

The steps here describe how to setup the VM from the command line. You can use
[virt-manager](http://virt-manager.org) if you prefer a graphical user
interface. Do submit your relevant virt-manager configs if you have some!

The steps here assume that your KVM host is also the desktop that you are
working from. If not, adapt the commands accordingly.

Installation steps:

1. Download Ubuntu 12.04 LTS 64 bit (`ubuntu-12.04.1-server-amd64.iso`) from
   http://www.ubuntu.com/download/server. For example, run the following
   commands within the Crowbar git checkout on the qemu-kvm host:

   ````
   cd dev-setup/qemu-kvm
   aria2c http://releases.ubuntu.com/precise/ubuntu-12.04.1-server-amd64.iso.torrent
   ````

1. Create a blank disk image that is at least 20 GB, eg:

   ````
   qemu-img create -f qcow2 -o preallocation=metadata ubuntu-12.04.qcow2 20G
   ````

1. Start a VM with the desired network (private network with NAT), with the ISO
   and disk attached. For example:

   ````
   sudo qemu-kvm -m 2G -daemonize -vnc :10 -cdrom ubuntu-12.04.1-server-amd64.iso \
                 -net nic,model=virtio,macaddr=DE:AD:BE:EF:30:22 \
                 -net tap,script=qemu-ifup \
                 -drive file=ubuntu-12.04.qcow2,cache=none,if=virtio
   ````

   Note that `script=qemu-ifup` points to the script at `qemu-kvm/qemu-ifup`,
   so make sure you are running the above command in the same directory, or
   modify it accordingly.

1. Connect to the VM via VNC and install the system:

   ````
   vncviewer :10
   ````

   The installer will attempt to auto-configure the network with DHCP, which
   you can cancel and jump to manual configuration instead with the following
   settings:

   ````
   IP address: 192.168.124.10
   Netmask:    255.255.255.0
   Gateway:    192.168.124.1
   ````

   Use the same name server (DNS) address as your host, which you can find out
   on Linux systems by running `grep nameserver /etc/resolv.conf` on the host.
   For example, within the SUSE intranet it is `10.120.2.88`. If the host is not
   running in any internal or corporate network, you can use `8.8.8.8`.

   The hostname and domain names can be left at the defaults. The apt-get proxy
   can also be left blank.

1. Once installation is complete, you can shutdown the VM (`sudo poweroff`) and
   subsequently start it in the same way, minus the `-cdrom ...` option. Or
   use the [qemu-kvm/start-vm](https://github.com/crowbar/crowbar/blob/master/dev-setup/qemu-kvm/start-vm))
   helper script.

## Setting up the development environment

You should now have a working VM that you can SSH into from the qemu-kvm host.
For example:

    ssh 192.168.124.10

The VM should also be able to access the external network. We can now start
with setting up the Crowbar development environment.

1. Install the basic Crowbar dev tool dependencies:

   ````
   sudo apt-get install git rubygems
   sudo gem install json --no-ri --no-rdoc
   ````

1. Copy your .gitconfig and other configuration files to the VM, eg:

   ````
   scp -r <your-usual-dev-host>:.{gitconfig,vimrc,vim,profile,ssh} .
   ````

1. Check out the Crowbar git repo and run the dev tool:

   ````
   git clone git://github.com/crowbar/crowbar.git
   cd crowbar
   ./dev setup
   ````

   The `./dev setup` script will ask for your Github username and password. It
   will fork the Crowbar and corresponding barclamp repositories to your
   account and clone them into `crowbar/barclamps/`. See [dev-and-workflow]
   (https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow)
   and [dev-and-code-review]
   (https://github.com/crowbar/crowbar/blob/master/README.dev-and-code-review)
   for details. This will take a while so get some coffee.

1. Install more dependencies for setting up and running the tests:

   ````
   sudo apt-get install libsqlite3-dev
   sudo gem install markdown kwalify rake bundler rcov rspec --no-ri --no-rdoc
   sudo gem install rails -v 3.2.10 --no-ri --no-rdoc
   ````

1. Running the dev tool to setup the Crowbar run-time environment at
   `/tmp/crowbar-dev-test`:

   ````
   ./dev setup-unit-tests
   ````

1. Running the unit and Rspec tests:

   ````
   cd /tmp/crowbar-dev-test/crowbar_framework
   bundle exec rake db:drop db:migrate db:fixtures:dump test:units spec
   ````

1. Running the BDD tests:

   ````
   cd /tmp/crowbar-dev-test/crowbar_framework/BDD
   ./linux_compile.sh
   ./linux_run.sh
   ````

   Note that the BDD tests require a running instance of Crowbar, which is
   started by `linux_run.sh`. If it fails with an error message like:

   ````
   ERROR: step run found error:{badmatch,{error,econnrefused}}
   ...
   ````

   This means that the Crowbar server is not running and can usually be fixed
   by running `linux_run.sh` again. Refer to the [BDD dev guide]
   (https://github.com/crowbar/barclamp-crowbar/blob/master/crowbar_framework/doc/default/crowbar/devguide/testing/bdd.md)
   for more details.

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
