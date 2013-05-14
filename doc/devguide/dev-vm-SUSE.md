# Crowbar Dev based on SUSE

Here we describe how to setup a Crowbar development environment in a virtual
machine (VM) that is based on openSUSE or SUSE Linux Enterprise Server (SLES).
It is currently focused on the core Rails application and required barclamps.

## Setting up the VM

We assume that you are using KVM and have read the [KVM setup
instructions](dev-vm.md).  If not, setup the VM accordingly and
continue on to the [next
section](#setting-up-the-development-environment).

We assume your KVM host is the desktop you are working from, so adapt them if
necessary.

### Download the VM

Download the latest version of the Crowbar Dev VM from one of the following
locations (KVM image in `qcow2` format recommended):

* [openSUSE Crowbar Dev VM](http://susestudio.com/a/n0rKOx/crowbar-dev)
* [SLES Crowbar Dev VM](http://susestudio.com/a/n0rKOx/crowbar-dev-sles)

### Boot the VM

This can be done *either* via `libvirt`, *or* directly via `qemu-kvm`:

#### Booting via libvirt

Give the VM a name; this can be whatever you want.

        kvm-host# vm_name=crowbar-admin-sles

Set which bridge device you want to route the VM's NAT traffic
through:

        kvm-host# bridge=virbr0

Register and boot the VM:

        kvm-host# vm-install \
                      -n $vm_name -o sles11 -c4 -m2048 -M2048 \
                      -d qcow2:$vm_disk,xvda,disk,w,0,cachemode=none \
                      -e \
                      --nic bridge=$bridge,model=virtio \
                      --keymap en-gb

(Note: the above applies for openSUSE 12.3.  Older distributions may
need tweaks, e.g. `cache=none` instead of `cachemode=none`.)

Of course you can tweak the number of virtual CPUs and amount of RAM
in the above arguments if you want.  It is not recommended to allocate
less than 2GB of RAM to the VM.

Once the VM is registered with `libvirt`, you can later control it
using `virt-manager` in the normal way.

#### Booting via qemu-kvm

1.  Place the image in the `dev-setup/qemu-kvm` directory of the [Crowbar
    git](https://github.com/crowbar/crowbar/) checkout on your KVM host.

1.  [Optional] Set which bridge device you want to route the VM's NAT
    traffic through:

            kvm-host# export BRIDGE=virbr0

1.  Start the VM by running the following as root:

            kvm-host# ./start-vm

    Use the `--preallocate` option if you need to improve disk performance.

### Post-boot

1.  [SLES VM only] Connect to the VM's graphical console to accept the
    end user license agreement (EULA).  If you used `qemu-kvm`,
    connect via VNC, e.g.:

            kvm-host> vncviewer :10

    Otherwise you will already see the console, but you can also connect
    via `virt-manager` or `vncviewer`.

    Once connected, type `q`, `y`, and hit enter.

1.  After the VM boots up (takes a bit longer for first boot), you
    should be able to connect to the VM via SSH:

            kvm-host> ssh root@192.168.124.10            # Password is 'linux'

1.  [SLES only] If you're running the VM within the SUSE network, run
    `add-suse-internal-repos` to add the internal SUSE
    repositories. Otherwise, if you have a SLES subscription, register
    with NCC to get updates.

1.  Create a non-root user account and set the password. Use the same
    username as you do on your regular workstation for
    convenience. Then re-login to the dev VM as the newly created
    user, e.g.:

            root@crowbar-dev> useradd -m jamestyj
            root@crowbar-dev> passwd jamestyj
            root@crowbar-dev> logout
            jamestyj@kvm-host> ssh 192.168.124.10
            jamestyj@crowbar-dev>

## Setting up the development environment

You should now have a working VM that you can SSH into from the qemu-kvm host.

1.  Copy your `.gitconfig` and other configuration files to the VM, e.g.:

            crowbar-dev> scp -r <your-usual-dev-host>:.{gitconfig,vimrc,vim,profile,ssh} .

1. Check out the Crowbar git repo and run the dev tool:

            crowbar-dev> git clone git://github.com/crowbar/crowbar.git
            crowbar-dev> cd crowbar
            crowbar-dev> ./dev setup

    The `./dev setup` script will ask for your Github username and
    password. It will fork the Crowbar and corresponding barclamp
    repositories to your account and clone them into
    `crowbar/barclamps/`. See
    [dev-and-workflow](https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow)
    and [dev-and-code-review](https://github.com/crowbar/crowbar/blob/master/README.dev-and-code-review)
    for details.

1.  Now assemble the Crowbar application:

            crowbar-dev> ./dev tests setup --no-gem-cache

    This assembles a working and testable Crowbar Rails application in
    `/tmp/crowbar-dev-test/opt/dell/crowbar_framework`.

1.  Now you can run an instance of the web UI:

            crowbar-dev> cd /tmp/crowbar-dev-test/opt/dell/crowbar_framework
            crowbar-dev> bundle install
            crowbar-dev> bundle exec rake db:migrate
            crowbar-dev> bundle exec rails s puma

1. And also to run the (unit + RSpec) tests:

            crowbar-dev> bundle exec rake db:drop railties:install:migrations db:migrate db:fixtures:dump test:units spec

See the [testing page](testing.md) for details.

## Troubleshooting tips

1.  Connect to the VM via VNC. This is useful for debugging the VM (e.g.
    networking issues).

            kvm-host> vncviewer :10

    The VM is configured with the following settings:

            IP address: 192.168.124.10
            Netmask:    255.255.255.0
            Gateway:    192.168.124.1
            DNS:        10.120.2.88, 8.8.8.8

    You may need to update the DNS setting to match your environment
    by modifying `/etc/resolv.conf`.
