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
     (http://HOSTNAME:5000) and from the `/opt/crowbar` directory.

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

     export pre_do_installcrowbar=$(base64 --wrap=0 <<'EOS'
       zypper ar -f http://dist.suse.de/install/SLP/SLE-12-SP1-SDK-LATEST/x86_64/DVD1/ sle12-sp1-sdk
       zypper -n in -l gcc ruby2.1-devel sqlite3-devel libxml2-devel;

       mkdir -p /opt/crowbar/crowbar_framework/db /opt/crowbar/barclamps;

       ln -sf /opt/dell/crowbar_framework/db/production.sqlite3 /opt/crowbar/crowbar_framework/db/development.sqlite3;
     EOS
     )

     export SHAREDVG=1
     export TESTHEAD=1
     export cloudsource=develcloud6
     export virtualcloud=l6
     export cloud=cloud6
     export net_fixed=192.168.116
     export net_public=192.168.96
     export net_admin=192.168.106
     export cephvolumenumber=0
     export nodenumber=2
     export vcpus=4
     export cloudvg=system
     export user_keyfile=~${SUDO_USER}/.ssh/id_rsa.pub
     export adminvcpus=2
     export want_sles12=1

     /usr/local/bin/mkcloud $@
     ```

  2. At first you need a running mkcloud setup. For more information about this
     read the [mkcloud](https://git.io/vYO2E) documentation. Please use the
     wrapper script created above to install Crowbar, e.g. `sudo mkcloud6 plain`.

  3. You need some ruby environment on you workstation in order to execute some
     rake tasks, so please install ```ruby``` and ```bundler``` first.

  4. You need to clone all repositories from GitHub. There are rake tasks for
     cloning, forking and updating all required repositories. Make sure to get
     all the dependencies with ```bundle install``` and use ```rake -T``` to get
     an overview about the rake tasks.

     1. ```rake crowbar:init``` will fork, clone and add the required remotes to the
        required repositories defined within our [configuration](../config/barclamps.yml)
        within [barclamps/](../barclamps/)
     2. ```rake crowbar:update``` will update the clones, this should be performed from time
        to time to get the latest changes into your cloned repositories.

  5. Now run Guard to sync your local git repos with the server, please execute
     ```GUARD_SYNC_HOST=192.168.106.10 bundle exec guard``` in a seperate
     terminal window as this process will stay in the foreground.

  6. Now ssh to the admin node and follow the steps below:

    1. Change to ```/opt/crowbar/crowbar_framework```.

    2. Set a dedicated GEM_HOME for the dev environment to avoid clobbering the
       admin node's system gems:

       ```
       export GEM_HOME=/tmp/crowbar-dev-gems
       ```

       You will need to ensure this environment variable is set each time
       you run the Rails server (step 5). You can also omit this step entirely,
       but without it the development environment is likely to break the admin
       node's regular crowbar setup. Likewise, you need to make sure that
       `GEM_HOME` is unset if you wish to use your system's regular gems again.

    3. Run ```bundle install```

    4. Install all barclamps with this snippet

       ```bash
       COMPONENTS=$(find /opt/crowbar/barclamps -mindepth 1 -maxdepth 1 -type d)
       CROWBAR_DIR=/opt/crowbar RAILS_ENV=development bundle exec /opt/crowbar/bin/barclamp_install.rb $COMPONENTS
       ```

    5. Run the Rails server ```bundle exec bin/rails s -b 0.0.0.0 -p 5000```


  7. Now you can access you crowbar development setup via ```http://192.168.106.10:5000```

### Troubleshooting
Nokogiri library might fail to install due to some missing dependencies (http://www.nokogiri.org/tutorials/installing_nokogiri.html), the following command can be used to resolve it:
```
zypper ar -f http://dist.suse.de/install/SLP/SLE-12-SP1-SDK-LATEST/x86_64/DVD1/ sle12-sp1-sdk
zypper -n in -l gcc ruby2.1-devel sqlite3-devel libxml2-devel
```
