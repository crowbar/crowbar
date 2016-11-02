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
     it in general at /usr/local/bin/mkcloud7

     ```bash
     #!/usr/bin/env bash

     export cloudsource=develcloud7
     export nodenumber=2
     export user_keyfile=~${SUDO_USER}/.ssh/id_rsa.pub

     /usr/local/bin/mkcloud $@
     ```

  2. At first you need a running mkcloud setup. For more information about this
     read the [mkcloud](https://git.io/vYO2E) documentation. Please use the
     wrapper script created above to install Crowbar, e.g. `sudo mkcloud7 plain devsetup`.
     the ```devsetup``` step for ```mkcloud``` is necessary to install the dependencies

  3. You need some ruby environment on you workstation in order to execute some
     rake tasks, so please install ```ruby``` and ```bundler``` first.

  4. You need to clone all repositories from GitHub. There are rake tasks for
     cloning, forking and updating all required repositories. Make sure to get
     all the dependencies with ```bundle install``` and use ```rake -T``` to get
     an overview about the rake tasks.
     For the ```rake``` tasks talk to the GitHub API you need to have a ```~/.netrc``` file
     which looks like:

     ```
     machine api.github.com
       login <GITHUB USERNAME>
       password <API TOKEN>
     ```

     The GitHub API Token can be obtained from your GitHub settings in the Browser.

     1. ```rake crowbar:init``` will fork, clone and add the required remotes to the
        required repositories defined within our [configuration](../config/barclamps.yml)
        within [barclamps/](../barclamps/)
     2. ```rake crowbar:update``` will update the clones, this should be performed from time
        to time to get the latest changes into your cloned repositories.

  5. Now run Guard to sync your local git repos with the server, please place this script in
     `/usr/local/bin/crowbar_guard`

     ```bash
     #!/bin/bash

     cloud=$1
     target_folder=$2
     guard_sync_port=$3

     if [[ -z $1 ]] || [[ -z $2 ]] || [[ -z $3 ]]
     then
       echo "usage ./crowbar_guard cloudname targetfolder sshport"
       echo "  example: ./crowbar_guard crowbarc1 /opt/crowbar 22"
       exit 1
     fi

     export GUARD_SYNC_PORT=$guard_sync_port
     export GUARD_SYNC_HOST=$cloud
     export GUARD_TREE_TARGET=$target_folder
     export GUARD_SCRIPT_TARGET=$target_folder/bin

     bundle exec guard --no-notify
     ```

     and run it in a seperate terminal window as this process will stay in the foreground.

     **NOTE**: you have to run the script **from the `crowbar/crowbar` git clone** where the `Guardfile` is located

  6. Now place the following script in `/usr/local/bin/crowbar_rails`

    ```bash
    #!/bin/bash

    cloud=$1
    port=$2
    targetdir=$3

    if [[ -z $1 ]] || [[ -z $2 ]] || [[ -z $3 ]]
    then
      echo "usage ./crowbar_rails cloudname targetdir"
      echo "  example: ./crowbar_rails crowbarc1 5000 /opt/crowbar"
      exit 1
    fi

    ssh -t root@${cloud} "$targetdir/crowbar_framework/bin/rails s -b 0.0.0.0 -p $port"
    ```

    and run it in a seperate terminal window as this process will stay in the foreground.

  7. Now you can access you crowbar development setup via ```http://your.crowbar.instance:5000```
