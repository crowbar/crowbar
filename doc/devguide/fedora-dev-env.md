# Crowbar dev environment based on Fedora

## Setting up the virtual machine (VM)

1. Create a new virtual network 192.168.124.0/24 (using virt-manager or virsh).
   Do not use DHCP for this network.

1. Start with a fresh Fedora 18 VM (can be minimal install).

1. Assign the previously created virtual network to the VM and configure the
   VM's networking like this:

   ````
   IP address: 192.168.124.10
   Netmask:    255.255.255.0
   Gateway:    192.168.124.1
   DNS:        192.168.124.1
   ````

   (You will need to edit `/etc/sysconfig/network-scripts/ifcfg-eth0` and
   `/etc/resolv.conf` to achieve this on a VM that was configured differently
   during installation.)

1. Connect to the machine via SSH and try to ping some address to verify that
   traffic gets routed correctly and DNS works.

   ````
   kvm-host> ssh root@192.168.124.10
   ````

1. [Optional] If you have problems with outbound connections from VM even after
   editing network-scripts and resolv.conf, it might be that iptables
   forwarding rules for the virtual network didn't get created on your host
   machine. Check that with iptables:

   ````
   kvm-host> sudo iptables -L

   Chain FORWARD
   target     prot opt source               destination
   ACCEPT     all  --  anywhere             192.168.124.0/24     state RELATED,ESTABLISHED
   ACCEPT     all  --  192.168.124.0/24     anywhere

   (The output is shortened to show the important part only.)
   ````

   If you don't see the above forwarding rules, shut down your VMs and restart
   libvirtd:

   ````
   kvm-host> systemctl restart libvirtd.service
   ````

   Then check the iptables output again and the forwarding rules should be there.

1. Create a user on the VM for Crowbar development.

   ````
   root@crowbar-dev> useradd -m crowbar
   root@crowbar-dev> passwd crowbar
   ````

1. Install dependencies that will be required to run the app in dev mode, build
   discovery image ("Sledgehammer") and run tests:

   ````
   root@crowbar-dev> yum install git ruby rubygem-rake rubygem-bundler mkisofs binutils markdown erlang debootstrap ruby-devel gcc gcc-c++ sqlite-devel libxml2-devel libxslt-devel
   ````


## Setting up the development environment

You should now have a working VM that you can SSH into from the host.

1. Copy your .gitconfig and other configuration files to the VM, e.g.:

   ````
   crowbar@crowbar-dev> scp -r <your-usual-dev-host>:.{gitconfig,vimrc,vim,profile,ssh} .
   ````

1. Check out the Crowbar git repo and run the dev tool:

   ````
   crowbar@crowbar-dev> git clone https://github.com/crowbar/crowbar.git
   crowbar@crowbar-dev> cd ~/crowbar
   crowbar@crowbar-dev> ./dev setup --no-github
   ````

   If you want the `./dev setup` script to automatically fork the crowbar and
   barclamp repositories into your github account, leave out the `--no-github`
   parameter.

   See [dev-and-workflow]
   (https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow)
   and [dev-and-code-review]
   (https://github.com/crowbar/crowbar/blob/master/README.dev-and-code-review)
   for details about the development process.

1. Now you can run the tests. See the [testing page](testing.md) for how to do
   it.

1. After you [generated the test setup](testing.md) into the
   `/tmp/crowbar-dev-test/opt/dell/crowbar_framework` directory, you can also
   [run the web UI](testing/web-ui.md) from there.

