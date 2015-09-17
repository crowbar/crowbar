# Crowbar: Development

This document describes the recommended setup for Crowbar front-end development
(eg. CSS, SASS, HAML, HTML, Rails).

## Background

For designers and (especially front-end) developers to have a productive
workflow, it is critical to be able to work directly from a Git checkout and
have, as much as possible, the exact same UI/UX as the actual product (and
upstream).

The challenge is that Crowbar is a complex product with many moving parts and
dependencies. The code is not structured or designed to run out of a Git
checkout. Many improvements have been made to simplify the running of the
Crowbar web UI from a Git checkout, but for a fully functional system (which one
needs to see and test all the UI and interactions), there is still a quite some
work.

In the following section we will describe in detail, the requirements of this
development setup and how it is configured.

## Flavors

We have the following two flavors of the Crowbar Web UI running so we can easily
compare the differences:

  1. __Regular SUSE OpenStack Cloud install__. Runs on standard http port
     (http://HOSTNAME) and from the `/opt/dell` directory.
  2. __From Git upstream branding__. Runs on [port 5000]
     (http://HOSTNAME:5100) and from the `/opt/crowbar` directory.

All are using the same default username and password, both ```crowbar```. The
following sections will describe in detail how each of these are setup and
configured.

### Installing

Before we can start you need to match some prerequirements on your host machine.

  1. Please create this script somewhere in your path, it's just a wrapper
     around our mkcloud script that will be mentioned later on again. We place
     it in general at /usr/local/bin/mkcloud6

     ```bash
     #!/usr/bin/env bash

     export pre_onadmin_installcrowbar=$(base64 --wrap=0 <<'EOS'
       zypper ar -f http://dist.suse.de/install/SLP/SLE-12-SDK-GM/x86_64/DVD1/ sle12-sdk
       zypper ref;
       zypper -n in -l gcc ruby2.1-devel sqlite3-devel libxml2-devel;

       mkdir -p /opt/crowbar/crowbar_framework/db;
       mkdir -p /opt/crowbar/barclamps;

       cp /opt/dell/crowbar_framework/db/production.sqlite3 /opt/crowbar/crowbar_framework/db/development.sqlite3;
       ln -sf /usr/bin/ruby.ruby2.1 /usr/bin/ruby
     EOS
     )

     export TESTHEAD=1
     export cloudsource=develcloud6
     export virtualcloud=wiggy
     export cloud=cloud6
     export net_fixed=192.168.116
     export net_public=192.168.96
     export net_admin=192.168.106
     export cephvolumenumber=0
     export nodenumber=2
     export vcpus=4
     export cloudvg=system

     /usr/local/bin/mkcloud $@
     ```

  1. At first you need a running mkcloud setup. For more information about this
     read the [mkcloud](http://git.io/vYO2E) documentation. Please use the
     wrapper script created above to install Crowbar.

  2. You need some ruby environment on you workstation in order to execute some
     rake tasks, so please install ```ruby``` and ```bundler``` first.

  3. You need to clone all repositories from GitHub. There are rake tasks for
     cloning, forking and updating all required repositories. Make sure to get
     all the dependencies with ```bundle install``` and use ```rake -T``` to get
     an overview about the rake tasks.

     1. ```rake crowbar:init``` will fork, clone and add the required remotes to the
        required repositories defined within our [configuration](../config/barclamps.yml)
        within [barclamps/](../barclamps/)
     2. ```rake crowbar:update``` will update the clones, this should be performed from time
        to time to get the latest changes into your cloned repositories.

  4. After this copy your ssh-key to the machine using ```ssh-copy-id```.

  5. Change within ```barclamps/crowbar/crowbar_framework/Gemfile``` the
     rubygems source from ```https``` to ```http``` as there is some known issue
     in SLE 11 for bundle install..

  6. Now run Guard to sync you local git clones with the server, please execute
     ```GUARD_SYNC_HOST=192.168.106.10 bundle exec guard``` in a seperate
     terminal window as this process will stay in the foreground.

  7. Now ssh to the admin node and follow the steps below:

    1. Change to ```/opt/crowbar/crowbar_framework```.

    2. Run ```bundle install```

    3. Install all barclamps with this snippet

       ```bash
       components=$(find /opt/crowbar/barclamps -mindepth 1 -maxdepth 1 -type d)
       CROWBAR_DIR=/opt/crowbar /opt/crowbar/bin/barclamp_install.rb $components
       ```

    4. shutdown the production server `systemctl stop crowbar && systemctl disable crowbar`
       as it currently causes confusion with the crowbar database. It gets out of sync when
       you e.g. change proposals.

    5. Run the Rails server ```bin/rails s -b 0.0.0.0 -p 5000```


  8. Now you can access you crowbar development setup via ```http://192.168.106.10:5000```
