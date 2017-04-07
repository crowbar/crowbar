# Barclamp development guide

This document is an introduction to Barclamp development. It will guide you
through all the steps neccessary to create a minimal (without UI elements)
Barclamp for Crowbar. It will then guide you through all the testing, CI and
review steps until the point where it gets merged into Crowbar.

## What is a Barclamp?

A barclamp is a plugin for Crowbar that configures a particular service or
group of services. It contains a Chef cookbook with one or more roles to
perform the actual configuration work and Crowbar UI code to parametrize this
chef cookbook and orchestrate applying its roles to individual machines.

## Terms and definitions

We will assume throughout this text that your barclamp is will be named
`mybarclamp`. Please substitute your actual barclamp's name for this
placeholder when you create a barclamp. The convention for OpenStack barclamps
is to use the OpenStack component's name, e.g. `nova`.

## Development Environment

This guide describes a very basic remote development environment on a Crowbar
admin node. You may skip this section entirely if you prefer working locally
(in this case you might want to look at our
[Guard based approach](https://github.com/crowbar/crowbar/blob/master/Guardfile)
and/or
[build-dist-rpms](https://github.com/openSUSE/pack-tools/tree/master/contrib/BS-pkg-testing)
for local development), or already use an entirely different development
environment.

The environment in this section serves three chief purposes:

* Provide something anybody can use even if their workstation accidently got an
  anvil dropped on it an hour ago and now they have a new machine fresh from IT
  with none of their customized personal environment on it. A `ssh` client is
  all you need for this guide.

* Document all the steps you need to go through to move your barclamp from a
  git working copy a point where it runs in Crowbar. This is helpful for both
  using (and debugging) existing development tools and for

* Last but not least it provides us with the handy `reposync_crowbar` command
  we will be using throughout this document. If you are using a different
  development environment, please substitute "ensure your code's current state
  of development is active in Crowbar" wherever this command occurs in the
  text.

With the example development environment, most of your development will take
place on your Crowbar node (log in as root).  You will find the currently
running Crowbar code (both Rails application components and chef recipes) in
the `/opt/dell` directory. We will provide detailed locations for all important
components of a Barclamp in the _Basic Implementation_ section below.

In order to track changes you make we recommend you keep a checkout of your
`crowbar-openstack` fork on the Crowbar node and apply the changes you make in
there


### Prerequisites

We will assume some things to already be present. Namely, the following:

* A Github fork of the appropriate Barclamp collection. For instance, you will
  find OpenStack related barclamps in the
  [crowbar-openstack](https://www.github.com/crowbar/crowbar-openstack)
  repository and high availability related Barclamps in the
  [crowbar-ha](https://github.com/crowbar/crowbar-ha) repository. For the sake
  of readability we will assume you are developing an OpenStack related and
  forked [crowbar-openstack](https://www.github.com/crowbar/crowbar-openstack)
  throughout the rest of this document.

* A Crowbar admin node with the Crowbar version you are developing against
  (for new barclamps this will be `master`) already installed and running. This
  should be as up-to-date as possible to reduce the chance of merge conflicts
  happening.

* A set of machines for your Crowbar admin node to orchestrate. In the case of
  SUSE OpenStack Cloud this would be a controller and one compute node at a
  minimum.

* You will need to install the following packages:

  * `patch`
  * `git-core`

Providing these is out of scope for this document. We assume you are able to
fork the appropriate Barclamp collection. The machines can be provided through
manual installation or by using an automated setup tool such as
[mkcloud](https://github.com/SUSE-Cloud/automation/blob/master/scripts/mkcloud).
Depending on the individual situation you can use either the `plain` or the
`instonly` `mkcloud` step. The former will install the nodes and deploy the
standard set of barclamps in their default configuration, while the latter will
only install the nodes. Use `plain` if your barclamp does not need to be
deployed before other barclamps and if you do not modify other barclamps.
Otherwise use `instonly` and deploy barclamps bit by bit through the Crowbar
web UI or the Crowbar CLI client. This will give you the opportunity to add
package repositories or tweak barclamps' proposal settings where needed along
the way.

Optionally (but strongly recommended) you can also

* Create a backup of your Crowbar admin node: You are probably going to break
  it a few times in the course of testing your new barclamp so it is a good
  idea to have a backup you can quickly restore rather than rebuild your entire
  test environment. If you are using `mkcloud` you can create and restore a LVM
  snapshot of your admin node using its `createadminsnapshot` and
  `restoreadminfromsnapshot` steps, respectively.

* Create an additional OpenStack controller or compute node as a scratch node:
  this will allow you to deploy only your own barclamp's controller side
  components to this node while all other OpenStack services reside on the main
  controller. This greatly shortens the time a chef run for your barclamp's
  controller side components takes and thus speeds up development. Make a
  backup of this node as well to guard against breakage and allow for
  clean-slate deployment of your barclamp's controller side components.

The best way to set up your scratch node is to make it a lonely node. To this
end, you will need to re-run mkcloud after you have created your cloud:

```
nodenumberlonelynode=1 mkcloud setuplonelynodes crowbar_register

```

### Preparations for Crowbar Node

These are recommendations, but we'll assume you followed them for the remainder
of this document since this shortens the commands in our examples. First of
all, clone your `crowbar-openstack` fork to `/root/crowbar-openstack` on your
Crowbar node and switch to your development branch. Note, that the code below
assumes you are working on a dedicated topic branch. It will break if you are
doing development on your fork's `master` branch. So do not work on `master`!

Next, create the following /root/.bashrc with a few useful aliases and
environment variables (substitute with appropriate values where so indicated by
comments):

```
# Replace "Crowbar Committer" by your name as it should appear in commit
# messages.
C_NAME="Crowbar Committer"

# Replace "email@address.invalid" by your email address as it should appear in
# commit messages.
C_EMAIL="email@address.invalid"

# Insert your own barclamp's name here:
export BARCLAMP_NAME=mybarclamp

# Insert a space delimited list of the Barclamps you are working on here (only
# required if your barclamp requires changes to other barclamps as well).

export BARCLAMPS="$BARCLAMP_NAME"

# Insert the path to your barclamp collection checkout here (in case it's
# somewhere other than /root/crowbar-openstack). This directory must be a git
# working copy of the crowbar-openstack fork and branch you are working on.

export BARCLAMP_FORK_PATH=/root/crowbar-openstack

export GIT_AUTHOR_NAME="$C_NAME"
export GIT_AUTHOR_EMAIL="$C_EMAIL"
export GIT_COMMITTER_NAME="$C_NAME"
export GIT_COMMITTER_EMAIL="$C_EMAIL"

# Applies all the committed changes in your crowbar-openstack working copy to
# /opt/dell and updates Crowbar runtime state. Note: this will break if you
# rebase your working copy, so do not rebase your working copy. If you rebase
# it (e.g. for final testing before a merge, please build a fresh Crowbar node
# or restore it from a snapshot).
function reposync_crowbar()
  {
  if [ -e /opt/dell/patchlevel ]; then
    . /opt/dell/patchlevel
    head=$(cd $BARCLAMP_FORK_PATH; git log --format='%H'| head -n 1)
    if [ "$head" = "$commit" ]; then
      echo "/opt/dell is already up to date. If this is not what you " 1>&2
      echo "expected to see, you may have uncommitted changes" 1>&2
      echo "in ${BARCLAMP_FORK_PATH}." 1>&2
      return
    fi
  fi

  pushd $BARCLAMP_FORK_PATH > /dev/null

  # This backup preserves the original /opt/dell from the RPM
  if [ ! -d /opt/dell.orig ]; then
    cp -a /opt/dell /opt/dell.orig
  fi

  # This backup preserves the /opt/dell from before the current
  # reposync_crowbar run. It is only preserved if reposync_crowbar
  # fails.
  if [ ! -d /opt/dell.bak ]; then
    cp -a /opt/dell /opt/dell.bak
  else
    echo "Found a previous intermediate backup of /opt/dell in /opt/dell.bak" 1>&2
    echo "This indicates a failed reposync_crowbar run." 1>&2
    echo "Please restore /opt/dell to a known good state from either of" 1>&2
    echo "    /opt/dell.bak" 1>&2
    echo " or /opt/dell.orig" 1>&2
    echo " and remove /opt/dell/back.bak once you have done so." 1>&2
    return 1
  fi
  # Note: $commit will only be set when sourced from /opt/dell/patchlevel.
  if [ -n "$commit" ]; then  # only apply commits added since the last reposync_crowbar run
    git diff $commit HEAD | patch -N --merge -p1 -d /opt/dell || return 1
  else # apply all commits in topic branch
    git diff origin/master...HEAD | patch -N --merge -p1 -d /opt/dell || return 1
  fi

  # Record current HEAD as patchlevel
  echo "commit=$(git log --format='%H'| head -n 1)" > /opt/dell/patchlevel
  popd > /dev/null

  for barclamp in $BARCLAMPS
    do
    if [ ! -h /opt/dell/crowbar_framework/barclamps/${barclamp}.yml ]; then
      ln -s /opt/dell/${barclamp}.yml /opt/dell/crowbar_framework/barclamps 2>&1 > /dev/null
    fi
    done
  chmod 755 /opt/dell/bin/*

  sync_crowbar
  rm -rf /opt/dell.bak
  }

# Helper function: updates Crowbar's runtime state after /opt/dell
# has been modified.
sync_crowbar()
  {
  barclamp_install.rb /opt/dell/crowbar_framework/barclamps &&
  for barclamp in $BARCLAMPS
    do
    knife cookbook upload -o /opt/dell/chef/cookbooks/ $barclamp
    done
  systemctl restart crowbar
  }

```

We will make heavy use of the `reposync_crowbar` command thus defined in the
following sections. A short explanation of the three:

`reposync_crowbar` will apply your whole fork against `/opt/dell` using
`patch(1)` and sync Crowbar's runtime state with what is now in /opt/dell. You
use this command to apply the current state of your fork to a freshly installed
Crowbar node. This command may cause merge conflicts which you will have to
resolve before proceeding. They usually happen due to a very out of date topic
branch. While you can resolve them in /opt/dell you are better off rebasing
your `crowbar-openstack` fork against upstream and applying the result to a
fresh Crowbar node. Also, you will find a backup copy of /opt/dell in
/opt/dell.orig if a merge conflict does occur.

Using patches against `/opt/dell` may seem a bit over-the-top at first since
one could simply copy files back and forth or use `barclamp_install` on your
checkout, but it prevents a lot of breakage in cases where `/opt/dell` is out
of sync with the `crowbar-openstack` repository's master branch or your own
fork of `crowbar-openstack` is out of sync with upstream.

In the former case copying files from your fork to `/opt/dell` or using
`barclamp_install` may cause an inconsistent state and errors entirely
unrelated to your change.

In the latter case copying files from `/opt/dell` to your fork may re-introduce
old code that is no longer present upstream. Both should not be an issue for
new barclamps, but it is better to get used to the `patch(1)` based workflow
right away since these problems do become a problem when changing an existing
barclamp later and thus the `patch(1)` based workflow becomes neccessary.

## Packages

If you create a new barclamp this is typically to deploy an OpenStack service
that does not exist on SUSE OpenStack Cloud, yet. While important, your
barclamp is only half of what is needed. The other half is creating the
packages your Barclamp will install the new OpenStack service from.

Ideally, you have already created these packages in [Open Build
Service](https://build.opensuse.org) and have taken the appropriate steps to
get them into the repository `mkcloud` uses for deployment (since `mkcloud`
uses SUSE internal repositories or a local copy of these internal repositories,
there is no point in detailing these steps in this public guide). In this case
you are all set, and your chef recipes will merrily install the packages they
need from official sources. If this isn't the case for you there are still some
strategies for getting your packages installed:

First of all, you will need a package repository to store your non-official
packages in. The easiest way to accomplish this is to register an account on
[Open Build Service](https://build.opensuse.org) and create a project with all
the packages you need under your home project. We'll assume you have this a
non-official package repository (or multiple repositories) with all the
packages you need in place for the remainder of this section.

If all you do is adding new packages, you should be fine with simply adding
your repository using `zypper ar` on every node you are installing your
packages to:

```
zypper ar -f http://download.opensuse.org/repositories/home:/<your OBS user>:/<your project name>/SLE_12_SP2 myrepo
zypper refresh
```

If you need to override existing packages with newer versions (for instance, if
the service your barclamp deploys requires a newer python-openstackclient
version you ship in your own repository), you may also need to disable vendor
locking for Zypper. To do this, set the following in `/etc/zypper/zypp.conf`:

```
solver.allowVendorChange = true

```

You need to do this because normally Zypper would prefer packages from official
repositories, even if your own repository contains newer version. If you set
this option the newer package from your repository will trump the one from
the official one.

Both steps must be performed on all relevant nodes _before_ you apply your
barclamp, otherwise applying it will fail due to Crowbar being unable to locate
your packages.

## Barclamp Component Overview

A barclamp consists of two main components:

* A [chef cookbook](https://docs.chef.io/cookbooks.html) which takes care of
  actually deploying your service.
* Some glue code to tie your chef cookbook into the Crowbar Web UI and backend.
  This glue code will parametrize your chef cookbook and apply its component
  roles to the nodes it orchestrates.

The subsequent sections will describe these two components and the ingredients
that go into them into detail. They start off with an overview of a barclamp's
development phases as we know them from experience and take you through all
aspects of barclamp implementation in the order you are likely to encounter
them. They are meant to be read in parallel to developing your own barclamp.

## Development Phases

In this section we will show a development workflow adapted for the remote
development environment we described in the beginning. This is not the
end-all-be-all of Barclamp development workflows, but it should be useful for
new developers. If you do not like this approach (for instance, if you prefer
developing in your local environment), our [Guard based
approach](https://github.com/crowbar/crowbar/blob/master/Guardfile) provides an
alternative. If your patch nears readiness, you may also find
[build-dist-rpms](https://github.com/openSUSE/pack-tools/tree/master/contrib/BS-pkg-testing)
useful. This tool will let you quickly build RPMs from your `crowbar-openstack`
fork.

The Development workflow essentially consists of 3 phases:

1. Basic Implementation: In this phase you will write a scaffold for the
   Barclamp's chef code, define roles and parameters and create UI code for
   parametrizing your Barclamp. Note that no testing needs to take place during
   this phase (and doesn't make much sense either). This phase produces a rough
   draft that may have any number of errors but will already define your
   barclamp's basic architecture.

2. Local Testing: in this phase you will debug and fix the rough draft from the
   first phase until the UI side of the barclamp works well and parametrizes
   the barclamp as expected. As a second step you will fill your chef recipe
   scaffold with life and develop your chef recipes to the point where they
   correctly deploy what they are supposed to deploy.

3. Pull Request Testing: once you are finished with phase 2 to the point where
   the barclamp performs to your own satisfaction you submit a pull request for
   your barclamp against the relevant Crowbar repository's `master` branch
   (e.g. against the `crowbar-openstack` `master` branch). Now Crowbar's CI
   tests will run using the code from your pull request and Crowbar maintainers
   will review it. During this phase you may again have to fix a number of
   things and may well need to return to phase 2 for local testing of the
   changes you make (it is not recommended to rely on CI tests by themselves
   for extensive changes).

Below we will describe these phases in detail.

_Note: unless otherwise indicated, all paths from here on out are relative to
your_ `crowbar-openstack` _fork's root directory._

### Basic Implementation

In this phase you will do a scaffold implementation of your barclamp. This
means that you will mostly focus on the Crowbar UI side of things and how it
ties into your barclamp's chef cookbook. This means that on the chef side
you'll only implement the structure of your chef cookbook, i.e. define the
parameters it takes and the roles it defines for the Crowbar UI to use.

You should also create recipes and configuration templates in your cookbook
already, but you only need to flesh these out as far as you need them to be
clear about the parameters you'll need Crowbar to supply.

Throughout this phase you can work on your local machine or on your development
Crowbar admin node, as you prefer, since the testing you can do in this phase is
limited to basic ruby syntax checks using `ruby -c`. Once you are finished with
this phase development will have to move to your Crowbar admin node, though.

#### Chef Cookbook

Location: `chef/cookbooks/`

The best point to start barclamp development is the
chef cookbook. Each barclamp contains
one of these. The chef cookbook is your barclamp's core component. It contains
the code that will do the grunt work of deploying the services, configuration
files and runtime state handled by your barclamp. As a first step, create
`chef/cookbooks/` now.

#### Cookbook metadata

Locations:

* `chef/cookbooks/mybarclamp/metadata.rb`
* `chef/cookbooks/mybarclamp/README.md`

Before we begin with the cookbook proper we will need to take care of some
boilerplate files. The first of these is `metadata.rb`. It mostly contains
various descriptive information about your barclamps. Here's an example from
the Barbican barclamp (`chef/cookbooks/barbican/metadata.rb`):

```
name "barbican"
maintainer "Crowbar project"
maintainer_email "crowbar@googlegroups.com"
license "Apache 2.0"
description "Installs/Configures Barbican"
long_description IO.read(File.join(File.dirname(__FILE__), "README.md"))
version "0.1"

depends "apache2"
depends "database"
depends "keystone"
depends "crowbar-openstack"
depends "crowbar-pacemaker"
depends "utils"
```

The contents of these fields are mostly free-form, hence you can copy the file
from another barclamp and simply adapt the `name` and `description` fields. The
one exception to that rule are the `depends` statements: these define which
other barclamps your own barclamp depends on because it uses their cookbooks'
contents. If, for instance, you use the Keystone barclamp's
`KeystoneHelper.keystone_settings()` method (which you should for OpenStack
barclamps), you'd add a `depends "keystone"` statement here.

You may already have noticed the second piece of boilerplate: the README.md
file referenced by the `long_description` statement. This one goes into
`chef/cookbooks/mybarclamp/` and describes your Chef cookbook as
verbosely or tersely as you like (please put at least one complete sentence in
there).

#### Recipes

Location: `chef/cookbooks/mybarclamp/recipes/*.rb`

[Recipes](https://docs.chef.io/recipes.html) make up the bulk of your chef
cookbook's payload. These recipes are written in Ruby (hence they must have a
`.rb` extension) and are the smallest organization unit of a chef cookbook.
They consist of [resource](https://docs.chef.io/resource.html) definitions that
define various aspects of system state, such as a configuration file's content,
packages to install, or a service to enable and start. In this step the recipes
do not need to contain any code, yet. You primarily need to know which recipes
to define and what each recipe's purpose is at this stage.

You are free to organize your recipes as you see fit. There are no
hard-and-fast rules for this since they mostly depend on the application your
barclamp deploys:

For instance, if your application (like most OpenStack components) consists of
an API service and a RabbitMQ driven backend service, you might want to split
these into a `api.rb` and `backend.rb` service. Likewise, if you need an
agent to run on compute nodes, this might warrant a dedicated `agent.rb`
recipe. If you want to use Crowbar's high availability features, it might also
make sense to have a dedicated `ha.rb` recipe for users deploying it in a
highly available manner.

On a more general note, feel free to take a look at the other cookbooks'
recipes in `chef/cookbooks/` for inspiration. We have barclamps for a range of
OpenStack services already and one of these might just match your own
application's architecture perfectly.

#### Roles

Locations:

* Role recipes: `chef/cookbooks/mybarclamp/recipes`
* Role definitions: `chef/roles/role_mybarclamp_*.rb`

Once you've got a set of chef recipes, you'll need to organize them into roles.
A role aggregates one or more chef recipes and deploys all of its component
chef recipes. Crowbar can assign roles to one or more nodes (subject to
constraints defined in the Crowbar application). In the HA case it can assign
roles to one or more clusters of nodes. It pays off to spend some time on getting
the roles you need right from the start because you will need to modify a lot
of code to add new roles or remove existing ones later. Roles vary in scope
from deploying just a single service (such as a logging agent) to deploying
multiple related services (such as all of an OpenStack service's controller
side daemons). All roles' unifying feature is that they are a container for
something that gets deployed to a certain type of machine which may among other
things be:

* "Any machine orchestrated by Crowbar"

* "Any OpenStack controller node"

* "Any OpenStack compute node"

* "Any Node in the Ceph Cluster"

* "Any Ceph OSD node"

We have a limited range of node types in Crowbar to define types of roles,
namely, these are the following:

* `compute`

* `controller`

* `network`

* `storage`

If you need a dedicated node type for your barclamp you will have to add it to
[crowbar-core](https://github.com/crowbar/crowbar-core) in the following
places:

* [`node_wall_list()` in `crowbar_framework/app/helpers/nodes_helper.rb`](https://github.com/crowbar/crowbar-core/blob/8a6f1c2c44717e3ceb066695312fef494e410f94/crowbar_framework/app/helpers/nodes_helper.rb#L361)
* [`role()` in `bin/crowbar_machines`](https://github.com/crowbar/crowbar-core/blob/8a6f1c2c44717e3ceb066695312fef494e410f94/bin/crowbar_machines#L358)
* [The list entry mappings in `crowbar_framework/config/locales/crowbar/en.yml`](https://github.com/crowbar/crowbar-core/blob/8a6f1c2c44717e3ceb066695312fef494e410f94/crowbar_framework/config/locales/crowbar/en.yml)

Roles are defined in two places in the chef cookbook. First, you create a _role
recipe_ in the `chef/cookbooks/<your_barclamp_name>/recipes` directory. Our
naming convention for role recipes is as follows:

```
role_<barclamp name>_<role description>.rb
```

`<role description>` may contain letters, numbers and underscore (`_`)
characters. Assuming you've got two separate role recipes, one for the compute
nodes and one for OpenStack controller nodes, you might end up with something
like this:

* `chef/cookbooks/mybarclamp/role_mybarclamp_compute.rb`
* `chef/cookbooks/mybarclamp/role_mybarclamp_controller.rb`

This role recipe contains one or more `include_recipe` statements to pull in
component recipes and a validity check that asks the Crowbar application
whether the role is valid for the current node. Here's an example from the
Barbican barclamp (`cookbooks/barbican/recipes/role_barbican_controller.rb`):

```
if CrowbarRoleRecipe.node_state_valid_for_role?(node, "barbican", "barbican-controller")
  include_recipe "barbican::api"
  include_recipe "barbican::common"
  include_recipe "barbican::worker"
  include_recipe "barbican::keystone-listener"
  include_recipe "barbican::ha"
end
```

Note how the recipes are qualified with the cookbook name ("barbican") in this
case and omit the `.rb` extension from their actual file name.

Once all role recipes are created, you can create the corresponding role
definitions in the `chef/roles` directory. Role definitions are snippets of
ruby code and must have a `.rb` definitions. Our naming convention for role
definitions is as follows:

```
<barclamp name>-<role description>
```

where `<role description>` is the role description from the role recipe with
underscores (`_`) substituted by dashes (`-`). This will be the role's
canonical name from this point onward.

A role definition looks as follows (example from `roles/barbican-controller.rb`):

```
name "barbican-controller"
description "Barbican Controller Role"
run_list("recipe[barbican::role_barbican_controller]")
default_attributes
override_attributes
```

Most of this is boilerplate, with two notable exceptions:

1. `name` defines the role's canonical name as described above
2. `run_list` contains a reference to the role's underlying role recipe.

Create role definitions for all of your roles now and proceed to the next
section.

#### Data Bag

Locations:

* Data bag schema: `chef/data_bags/crowbar/template-mybarclamp.schema`
* Default data bag: `chef/data_bags/crowbar/template-mybarclamp.json`
* Migrations: `chef/data_bags/crowbar/migrate/mybarclamp/*.rb`

Now that you have your recipes and roles defined you should also know what
configuration settings are static (e.g. best practice settings you want to
hardwire as enabled or disabled) and what needs to go into your chef cookbooks
as parameters that can vary for each local setup (e.g. user names and passwords
or the OpenStack identity URL). Everything you want to be configurable will
need to go into the `data bag`.

For a newly created Barclamp the data bag consists of a _data bag schema_
(`chef/data_bags/crowbar/template-mybarclamp.schema`) and a
_default data bag_ (`chef/data_bags/crowbar/template-mybarclamp.json`). Both
are in JSON format. The _data bag schema_ describes how your data bag is
structured and imposes constraints such as data type and permissible values on
individual fields (much like an XML schema). It is used to validate your
barclamp's parameters, especially when the user edits its parameters directly
as JSON, either in the Web UI's raw mode or through the Crowbar command line
client.

The _default data bag_ contains default settings for these parameters where
defaults can reasonably be provided. There are some special cases such as user
names and passwords that are initially left blank and will be filled in by the
Crowbar web UI when the barclamp is activated, so feel free to leave fields
blank if you plan on adding code to the Crowbar web UI to populate them or make
use of existing web UI.

When fields are added to or removed from an existing barclamp's data bag, you
will need to add another component to the data bag: a _migration_. This is
essentially a database migration for the Crowbar web UI since data bags map
directly to a table in the Crowbar web UI's database. To keep that database
in sync with the data bag you will need to provide migrations to update that
database table if you make changes to an existing barclamp's data bag.

In the following sections we will explain the data bag's components in detail.
We will start with the default data bag rather than the data bag schema because
it is easier to fill that with content first, and then produce a schema to match.

##### Default Data Bag

* Location: `chef/data_bags/crowbar/template-mybarclamp.json`

First of all, copy an existing default data bag to
`chef/data_bags/crowbar/template-mybarclamp.json`. You will modify
this data bag to contain your own barclamp's parameters. This is easier and far
less error prone than writing a default data bag from scratch. Throughout this
section I will assume you used the
[Barbican barclamp's default data bag](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/data_bags/crowbar/template-barbican.json)
and link to it when we discuss the changes you need to make.

First of all, replace all occurrences of `barbican` by `` and adjust
the top-level `description field`. Your default data bag's first few lines
should now look something like this:

```
{
  "id": "template-",
  "description": "The Foobar as a Service Component of OpenStack",
  "attributes": {
      "" : {
```

The corresponding section of the Barbican default data bag is
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/data_bags/crowbar/template-barbican.json#L1:L29)

With these preliminaries out of the way you can now add all the parameters you
need under `attributes[""]` and remove existing parameters left over
from the default data bag you copied. You should leave the following fields in
place, since the Crowbar UI will automatically populate them for barclamps in
`crowbar-openstack` and/or because some chef side helper functions will use
them:

* `db`
* `database_instance`
* `keystone_instance`
* `rabbitmq_instance`
* `user`
* `group`
* `service_password`
* `service_user`

When you are done `attributes[""]` should look something like this:

```
     "" : {
        "api" : {
           "bind_host" : "*",
           "bind_port" : 12345,
           "logfile" : "/var/log/myservice/myservice-api.log"
        },
        "backend" : {
            db_plugin = "postgres",
            enable_foo = true,
            foobar_threshhold = 255
        },
        "db" : {
           "database" : "myservice",
           "password" : "",
           "user" : "myservice"
        },
        "debug" : false,
        "group" : "",
        "user" : "",
        "database_instance": "none",
        "keystone_instance": "none",
        "rabbitmq_instance": "none",
        "service_password": "none",
        "service_user": "myservice"
     }
```

The corresponding section of the Barbican default data bag is
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/data_bags/crowbar/template-barbican.json#L5:L29)

Once you have added your parameters you will need to adjust
`deployment[""]`. This section provides meta data about your chef
cookbook to the Crowbar web UI. The following fields need to be changed in
there:

* `schema-revision`: This field contains the data bag's schema revision. For a
   new barclamp it starts with the `crowbar-core` `master` branch's current
   base revision (at the time of this writing this was `100` but it increases
   with every new stable Crowbar release. Please ask a Crowbar developer about
   the current value). Whenever the data bag is changed, this revision is
   incremented by `1` and a corresponding migration is added to the
   `chef/data_bags/crowbar/migrate/mybarclamp/` directory. Since you are
   writing a new barclamp, set this to  the current `master` branch's base
   revision.

* `element_states`: this field contains a mapping from your chef roles to a
  list of valid states for each role. Typically these states are `readying`,
  `ready` and `applying` for every role, so just use these values for every
  role's list.

* `element_order`: this field is a list of single-element lists and defines the
  order in which your barclamp's roles should be applied. Each single-element
  list contains a role name. Whichever role occurs first in this list must have
  been applied successfully to all nodes it is assigned to before the roles
  further down are applied to their respective nodes. In other words, this
  controls global execution order of your roles across all nodes orchestrated
  by Crowbar. All of your roles should be mentioned here.

* `element_run_list_order`: this field governs the priority of your roles in
  relation to the other roles assigned to any given node your own role is
  assigned to. In other words, this governs local execution order of all roles
  (i.e. not just yours) assigned to a given node. All of your roles should be
  assigned a priority here.

When you are done `deployment[""]` should look something like this
(this example assumes you have defined a `mybarclamp-compute` and a
`mybarclamp-controller` role):

```
  "deployment": {
    "" : {
      "crowbar-revision": 0,
      "crowbar-applied": false,
      "schema-revision": 100,
      "element_states": {
        "mybarclamp-controller": [ "readying", "ready", "applying" ],
        "mybarclamp-compute": [ "readying", "ready", "applying" ]
      },
      "elements": {},
      "element_order": [
        [ "mybarclamp-controller" ],
        [ "mybarclamp-compute" ]
      ],
      "element_run_list_order": {
        "mybarclamp-controller": 120,
        mybarclamp"-compute": 120
      },
      "config": {
        "environment": "mybarclamp-base-config",
        "mode": "full",
        "transitions": false,
        "transition_list": []
      }
    }
  }
```

The corresponding section of the Barbican default data bag is
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/data_bags/crowbar/template-barbican.json#L31:L53)

##### Data bag schema

* Location: `chef/data_bags/crowbar/template-mybarclamp.schema`

With your default data bag finished you can now use the attributes and their
data types from the default data bag as a guide for definining its schema. The
schema holds constraints for every attribute in the default data bag, plus
for optional attributes that may not be set in the default data bag but can
nonetheless be added by the user.

The schema contains a hierarchy of dictionaries that mirror the data structure
in `template-mybarclamp.json`. The keys in this hierarchy are the attribute
names from `template-mybarclamp.json` and the values are dictionaries of
constraints for the attribute in question. Such a dictionary of constraints can
have the following fields (these are only the most common ones):

* `required`: a boolean value indicating whether this attribute must be present
  (`true`) or not (`false`).

* `type`: the attribute's data type. Valid values are `str`, `bool`, `int`,
  `map` (key/value pairs) and `seq` (lists of values).

* `pattern` [only for attributes of type `str`]: a regular expression the attribute
  must match.

* `mapping` and `sequence` [only for attributes of type `map` and `sequence`,
  respectively]: these contain lists or dictionaries that can be used to impose
  constraints on a list or dictionary value's contents. This is mainly used to
  cover the whole tree of nested data dictionaries in the data bag, but it can
  also be used to impose restrictions on an individual list or dictionary
  attribute at the lowest level.

Armed with this knowledge we can now create a schema that matches the default
data bag from the previous section. Similar to the previous section we will
start out with a copy of the Barbican barclamp's schema
(`template-barbican.schema`), which we'll copy to `template-mybarclamp.json`.

Again we will first globally search and replace `barbican` by `` and
then go through it section by section, starting with the top-level fields. The
only thing that needs adjusting at the top level is the pattern constraint for
the `id` field, which our global search and replace will already have taken
care of, leaving us with a top-level section that should look something like this:

```
{
  "required": true,
  "type": "map",
  "mapping": {
    "id": { "type": "str", "required": true, "pattern": "/^-|^template-mybarclamp$/" },
    "description": { "required": true, "type": "str" },
    "attributes": {
      "required": true,
      "type": "map",
      "mapping": {
        "": {
```

The corresponding section from the Barbican data bag schema is
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/data_bags/crowbar/template-barbican.schema#L1:L11)

Next, we will need to adjust the constraints for `attributes[""]`. The Crowbar
web UI is a Ruby on Rails application with a MVC (Model-View-Controller)
architecture so you will need to create the MVC architecture's three
components for your barclamp plus some glue and metadata. This section will
take you through all of these steps.

##### Barclamp Metadata

Location: `mybarclamp.yml`

Much like the chef cookbook, the Crowbar application needs some metadata about
your barclamp. This is a YAML formatted file residing in the top level
directory. It's filename consists of your barclamp's name plus the extension
`.yml`. Below you will find a commented example:

```
barclamp:
  # Your barclamp's name as it appears in file names.
  name: 'mybarclamp'
  # Your barclamp's name as it will be displayed in the Crowbar web UI.
  display: 'Mybarclamp'
  # Here we usually have the service name and a short description of the
  # Service the barclamp deploys for OpenStack barclamps.
  description: 'OpenStack Myservice: Provides Foo and Bar'
  # Just leave this as it is.
  version: 0
  # This is a repeat of the requirements list from the chef cookbook.
  requires:
    - '@crowbar'
    - 'database'
    - 'rabbitmq'
    - 'keystone'
  # This denotes the Barclamp collection this barclamp belongs to.
  member:
    - 'openstack'

crowbar:
  # The barclamp's priority in the crowbar UI (lower number means higher priority)
  run_order: 98
  # The barclamp's priority in the ordering of chef cookbooks (lower number
  # means higher priority)
  chef_order: 98

  # FIXME: The following appear to be unused. They are neither loaded in
  # catalog() in barclamp_mgmt_lib.rb, nor do they have a corresponding method
  # in the BarclampCatalog class). You might be able to omit them.
  order: 98
  proposal_schema_version: 3
  layout: 1
```

##### Command Line Executable

Location: `bin/crowbar_mybarclamp`

This executable is used by the crowbar command line client and is mostly a
stub. Just copy this from another barclamp and change the `@barclamp` variable
to your barclamp's name, which will leave you with something like this for our
example barclamp:

```
require File.join(File.expand_path(File.dirname(__FILE__)), "barclamp_lib")
@barclamp = "mybarclamp"
@timeout = 3600

main
```

##### Minimal Edit View

Locations:

* `crowbar_framework/app/views/barclamp/mybarclamp/_edit_attributes.html.haml`
* `crowbar_framework/config/locales/mybarclamp/en.yml`

Since we are building a minimal barclamp without much in the way of UI
components, we can stick to the bare minimum for the "View" part of MVC. We
only need its components to be present without much content.

First of all we create the directories

* `crowbar_framework/app/views/barclamp/mybarclamp`
* `crowbar_framework/config/locales/mybarclamp`

Next, we create the basic view in
`crowbar_framework/app/views/barclamp/mybarclamp/_edit_attributes.html.haml`.
Since all we need is an edit field for raw parameters in JSON format, we can
get away with only having the following snippet of code in there:

```
= attributes_for @proposal do
  .panel-sub
      = header show_raw_deployment?, true
```

The corresponding file from the Barbican barclamp is
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/crowbar_framework/app/views/barclamp/barbican/_edit_attributes.html.haml).


What remains for the view is a minimum English language locale in
`crowbar_framework/config/locales/mybarclamp/en.yml`. Since we do not display
any form fields this can be fairly short. To give you an idea of how an actual
form field's description would look we included one in our example below (note
that this would require an entry in
`crowbar_framework/app/views/barclamp/mybarclamp/_edit_attributes.html.haml` to
actually show up):

```
en:
  barclamp:
    mybarclamp:
      edit_attributes:
        api_header: 'API Settings'
        api:
          bind_host: 'Address to listen on'

```

The corresponding file from the Barbican barclamp is
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/crowbar_framework/config/locales/barbican/en.yml).
It contains field descriptions for a bunch of fields already, since this will
make it easier to add UI elements later. Feel free to prepare your own barclamp
in a similar manner.

##### Minimal Model Class

Location: `crowbar_framework/app/models/mybarclamp_service.rb`

With the view in place we need a model class to pre-process the proposal
information and enforce constraints that cannot be expressed using the
declarative means available in `template-mybarclamp.schema`, i.e. constraints
that need program logic to check. Just like in the data bag sections we will
again use the Barbican barclamp's proposal as an example and modify it bit by
bit, so as a first step, copy `crowbar_framework/app/models/barbican_service.rb`
to `crowbar_framework/app/models/mybarclamp_service.rb` and globally replace
`barbican` by `mybarclamp` and `Barbican` by `Mybarclamp`, respectively.

This will leave us with with something like this in the first few lines:

```
class MybarclampService < PacemakerServiceObject
  def initialize(thelogger)
    @bc_name = "mybarclamp"
    @logger = thelogger
  end
```

The corresponding lines from the Barbican barclamp's controller are
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/crowbar_framework/app/models/barbican_service.rb#L17:L21)

The next thing we need to change is the `role_constraints` method. This method
tells Crowbar on how many nodes the service can be deployed, what types of node
are valid and on which platform it may run. All the roles you defined in the
chef sections need to be accounted for by this method. This means that we will
need to add an entry for the `mybarclamp-compute` role. Below you will see a
commented example:

```
    def role_constraints
      {
        "mybarclamp-controller" => {
          "unique" => false,   # Set to `true` if this can only be deployed to one node.
          "count" => 3,        # Number of allowed instances ('-1' means infinite)
          "cluster" => true,   # Whether this role can be clustered (important for HA later on).
          "admin" => false,    # Whether this role can be assigned to the Crowbar admin node.
          "exclude_platform" => {
            "suse" => "< 12.1",  # Do not deploy on SUSE version 12.1 and lower.
            "windows" => "/.*/"  # Do not deploy on any Windows platforms.
          }
        },
         "mybarclamp-compute" => {
          "unique" => false,
          "count" => -1,   # Can be deployed to an arbitrary number of Compute nodes
          "cluster" => true,
          "admin" => false,
          "exclude_platform" => {
            "suse" => "< 12.1",
            "windows" => "/.*/"
          }
        }
      end
    end
```
The corresponding lines from the Barbican barclamp's controller are
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/crowbar_framework/app/models/barbican_service.rb#L29:L43)

Depending on your barclamp's dependencies you may need to adjust the list of
dependencies returned by the `proposal_dependencies()` method. Since `mybarclamp`
has the same dependencies as the Barbican barclamp, that is not neccessary in
our case. You will find the Barbican barclamp's relevant line
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/crowbar_framework/app/models/barbican_service.rb#L47).

Now we come to the method where we'll usually have most of our changes:
`create_proposal()`. This method creates the run-time proposal in Crowbar's
data base that contains the parameters as they will be passed to your chef
recipe. It has two main purposes:

1. Allocate roles to nodes automatically as they make sense (i.e. assign
   `mybarclamp-controller` to a one or more designated controller nodes and assign
   `mybarclamp-compute` to all compute nodes).

2. Populate fields that you left blank in your default data bag (e.g. generate
   random passwords).

After adaption our method might look something like this:

```
  def create_proposal
    @logger.debug("Mybarclamp create_proposal: entering")
    base = super

    nodes = NodeObject.all
    server_nodes = nodes.select { |n| n.intended_role == "controller" }
    server_nodes = [nodes.first] if server_nodes.empty?

    compute_nodes = nodes.select { |n| n.intended_role == "compute" }
    compute_nodes = [nodes.first] if compute_nodes.empty?

    base["deployment"][@bc_name]["elements"] = {
      "mybarclamp-controller" => [server_nodes.first.name]
    } unless server_nodes.nil?

    base["deployment"][@bc_name]["elements"] = {
      "mybarclamp-compute" => compute_nodes
    } unless compute_nodes.nil?


    base["attributes"][@bc_name]["database_instance"] =
      find_dep_proposal("database")
    base["attributes"][@bc_name]["rabbitmq_instance"] =
      find_dep_proposal("rabbitmq")
    base["attributes"][@bc_name]["keystone_instance"] =
      find_dep_proposal("keystone")
    base["attributes"][@bc_name]["service_password"] = random_password
    base["attributes"][@bc_name][:db][:password] = random_password

    @logger.debug("Mybarclamp create_proposal: exiting")
    base
  end
```

The Barbican barclamp's `create_proposal` method is
[here](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/crowbar_framework/app/models/barbican_service.rb#L57:L81)

##### Minimal Controller Class

Location: `crowbar_framework/app/controllers/mybarclamp_controller.rb`

The final thing we will need to the Crowbar application is a controller. This
goes into `crowbar_framework/app/controllers/mybarclamp_controller.rb` and is
very straightforward since we only need stub code in there (we normally don't
do much in Barclamp controllers). This will suffice:

```
class MybarclampController < BarclampController
  # Controller for Mybarclamp barclamp

  protected

  def initialize_service
    @service_object = MybarclampService.new logger
  end
end
```

As you can see, Barbican has the same [piece of boilerplate
code](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/crowbar_framework/app/controllers/barbican_controller.rb).

### Local Testing

*Note: Whenever you make changes during this phase, make them to the
`crowbar-openstack` git checkout in `/root/crowbar-openstack`, commit them, and
then run `reposync_crowbar` to make them known to Crowbar.

In the previous sections you created a scaffold for your barclamp, possibly
with some pieces of implementation as well. Before you create a pull request
for that code you should test it locally now. While there is a school of
thought that advocates creating pull requests as soon as the first commit is
in, this approach is problematic for Crowbar development: one of our CI tests
is the [Hound](https://houndci.com/) code style checker, which will generate a
notification for every single violation it detects. Since each commit is likely
to contain multiple such violations, it is better to get your code working
first and then present it to Hound at that point. If you test and debug your
barclamp iteratively while the pull request is already open you will likely
generate a lot of needless spam (e.g. for code that may end up eventually
getting removed entirely in the course of development)

For testing your code locally you will need the development environment
described in the [Prerequisites](#prerequisites) section. So build such an
environment now (if you haven't already done so) and make sure you've got an
up-to-date checkout of your `crowbar-openstack` fork in
`/root/openstack-crowbar` on the development environment's Crowbar admin node.
This is also a good time to create a LVM snapshot or other backup of your
Crowbar admin node in case you break it in the course of debugging.

With your Crowbar node backed up and a checkout of your `crowbar-openstack`
fork in place, apply your code to the running Crowbar instance using the
commands from the [Preparations for Crowbar Node](#preparations-for-crowbar-node)
section:

```
reposync_crowbar   # Fix any merge conflicts reported by this command before proceeding
```

If you navigate to your Crowbar Admin Node's web interface now and select
*OpenStack* from the barclamp menu, you should see your new barclamp listed. If
it doesn't appear or you got an error message from the `sync_crowbar` command
you will now have to start debugging. Common errors at this stage are mostly
bad file names, or problems in `mybarclamp.yml`.

#### Create Proposal

Once your barclamp appears in the list of barclamps, the next testing step is
creating a *proposal*: this means creating a run-time configuration for the
Barclamp and saving it.

First of all, click on the `Create` button for your Barclamp in the list of
barclamps. If this fails the reason is usually due to one of the following:

* Errors in `crowbar_framework/app/models/mybarclamp_service.rb`
* Syntax or naming errors in `crowbar_framework/app/controllers/mybarclamp_controller.rb`
  (very uncommon since it's just boilerplate that can be produced through a
  simple search-and-replace operation on another barclamp's controller)
* `crowbar_framework/app/views/barclamp/mybarclamp/_edit_attributes.html.haml`
* Errors in your default data bag schema, e.g.`chef/data_bags/crowbar/template-mybarclamp.json`.
  This is what error messages about invalid JSON syntax usually refer to.

Upon failure the Crowbar web UI will usually (but not in all cases) display a
stack trace that will lead you to the problematic file and at least hint at
what the problem is. Once creating the proposal succeed you will see the
barclamp's view where you will be able to edit the proposal.

#### Save Proposal

The next hurdle for your barclamp is saving the proposal (runtime
configuration) Crowbar created from its default data bag. To test this, click
the `Save` button in the barclamp's view you reached at the end of the previous
step. This will cause the automatically generated proposal to be validated and
saved to Crowbar's database. If this fails this is usually due to one of the
following reasons:

* Type mismatches between `template-mybarclamp.json` and
  `template-mybarclamp.schema`. This is the most common error.

* Logical errors in `crowbar_framework/app/models/mybarclamp_service.rb`. These
  most commonly are role constraint mismatches or fields required by the
  schema but not filled in by the model.

Once you have successfully saved your proposal you can advance to the final
step of local testing: applying the proposal.

#### Apply Proposal

Depending on how elaborate your chef recipes are at this stage this step can
either be very short or very long. To start testing you simply click the
*Apply* button in your barclamp's view and wait for the Crowbar Web UI to
report back to you. This may take a fair amount of time. Here's why:

As you hit *Apply*, the chef roles you defined will be added to the run list
for the nodes they are assigned to. That run list is usually not empty, though.
For instance, an OpenStack controller usually has about 20 roles in its run
list, to which your own gets added. Compute nodes usually have a lot less. All
these roles are now applied and chef checks *all* of their resources to make
sure they are still in the desired state at a minimum (and if they are not it
adjusts them).

Needless to say, the whole process takes a long time. This is where the scratch
node from the [Prerequisites](#prerequisites) section comes in. Instead of
assigning your roles controller side role(s) to the controller all the other
roles are assigned to, you assign it to the second designated controller node,
which will only have your own barclamp in its run list, which will speed up
applying your barclamp a great deal.

If the proposal applied successfully you can move on to the next step (or even
skip it if your chef recipes already contained everything you needed to deploy
your application at this point). If it fails to apply at some stage you will
get a stack trace that tells you exactly which chef recipe went wrong on which
line.

#### Implement and Debug Chef Recipes

The final step is fleshing out your chef recipes if you haven't already done
so. We cannot offer much guidance at this point, because this mostly requires
your own knowledge of the application you are deploying. On the Crowbar side
there is not that much documentation, unfortunately, so your best bet is
studying existing barclamps' chef recipes. That being said, here are some leads
to documentation and useful helpers that will aid you in cookbook development:

* [Upstream documentation on Chef Cookbooks](https://docs.chef.io/cookbooks.html): 
  note that we are using a fairly old Chef version, so some of the information
  you will find there will not apply. Documentation on basic resources should
  be fine, though.

* [KeystoneHelper](https://github.com/crowbar/crowbar-openstack/blob/master/chef/cookbooks/keystone/libraries/helpers.rb):
  this library is part of the Keystone barclamp and contains various useful
  methods you can use in your recipes. The most useful is probably
  `keystone_settings()` which will provide you with the credentials for a
  service account you can use in your service's configuration files. See the
  Barbican barclamp's [common.rb recipe](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/cookbooks/barbican/recipes/common.rb#L116)
  for a usage example.

* [fetch_database_settings()](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/cookbooks/crowbar-openstack/libraries/helpers.rb#L23):
  This method from the `crowbar-openstack` barclamp provides your recipes with
  a database username and an automatically generated password, and other
  database settings such as your database's instance's host name, port and
  database type. Again, see the Barbican barclamp for a 
  [usage example](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/cookbooks/crowbar-openstack/libraries/helpers.rb#L23).

* `database`, `database_user`: these are resource types defined in the 
  [database barclamp](https://github.com/crowbar/crowbar-openstack/tree/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/cookbooks/database/libraries).
  They allow you to create databases and database user accounts to access them.
  The Barbican barclamp provides a [usage example](for these resource types as
  well.

* `crowbar_openstack_wsgi`: this is a resource defined in the 
  [crowbar-openstack barclamp](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/cookbooks/crowbar-openstack/providers/wsgi.rb)
  and allows you to easily deploy a WSGI application. We use this for all
  OpenStack API services. Once more the Barbican barclamp will serve as a 
  [usage example](https://github.com/crowbar/crowbar-openstack/blob/8bebf8a379ebea8ef462ad49746dda6d36a3c46d/chef/cookbooks/barbican/recipes/api.rb#L127).

You will have to iterate on this step until the application deploys to your
satisfaction. Once it does (and you have preferably tested it against a test
setup rebuilt from scratch) you are finally ready to submit a pull request.

### Pull Request Testing

Now that your new barclamp deploys your application successfully, you can
submit a GitHub pull request against the `crowbar-openstack` `master` branch.
This also applies if you are targetting a stable Crowbar release: you will
still need to submit a pull request against `master` first and then cherry-pick
the change in `master` to the stable branch in a separate pull request.

Once the pull request is submitted, various CI tests will run and reviewers
from the core Crowbar team will eventually take a look as well. This is likely
to result in further changes for anything non-trivial. If you make any
non-trivial changes at this stage (and perhaps even then), please make sure
they didn't break anything by repeating local testing with these changes in
place.

#### Hound Style Check

The simplest of our CI tests is a [Hound](https://houndci.com/) Ruby code style
check. It is very pedantic and will usually find something to object to in the
cleanest pull request (even Crowbar team members usually generate multiple
Hound violations in the first iteration of a new pull request). It will
annotate all lines in your code that fail its style checks. Address all of
these comments and update your pull-request. Hound will re-examine it and may
find new problems. Re-iterate until Hound passes without any objections.

##### Manual Ruby Style Check

It is possible to run the Ruby code style checks manually, before submitting 
the GitHub pull request, or any other time. The [rubocop-git](https://github.com/m4i/rubocop-git) 
tool available on Rubygems may be used to run code style checks against one or 
more git commits or even on staged files. The Ruby code style definition files 
used by Hound are hosted in the root of the [crowbar](https://www.github.com/crowbar/crowbar-openstack) 
Github repository and they are required as input by rubocop-git.

Install:

```
  sudo gem install rubocop rubocop-git
```

Example invocation:

```
  rubocop-git.ruby2.1 -c /path/to/crowbar/.hound.ruby.yml e7919ec6584a1b71dacab9c0a437cb07693edb02
```

Usage notes/problems:

* it must be run in the working copy's root directory, otherwise it won't find
  the files to run its checks against.
* a commit ID (the one before your own changes, usually) must always be
  supplied. It does _not_ default to using the result of a `git show` if the
  commit ID is omitted. Instead, it uses the updated files in the git working copy.

#### Mkcloud Gating

While it is mandatory for a pull request to get merged, this test currently
doesn't run for all pull requests. `mkcloud` gate jobs will currently only run
automatically for pull requests submitted by members of the Crowbar core
development team or people granted explicit permission to run them. If that is
not the case for you, you will have to ask a member of the Crowbar team to help
you since a passing `mkcloud` gate is mandatory for all changes to Crowbar and
its barclamps.

The `mkcloud` gating uses `mkcloud` to deploy SUSE OpenStack Cloud with Crowbar
patched to include your pull request. If this succeeds, the `mkcloud` gate
passes. 

There is a catch though: since your barclamp is new, the `mkcloud` gate will
not automatically enable it. Consequently, the `mkcloud` gate will pass anyway
if your barclamp did not actively break other barclamps. This level of scrutiny
is fine for a new barclamp, but once your barclamp is merged, you will need to
add test automation. See the section [Adding Test Automation](#adding-test-automation) 
for detailed instructions on how to do this after your barlcamp merges. Adding
(or rather enabling) test automation for your barclamp would not make much
sense before that point: since the barclamp is not available _all_ our
`mkcloud` testing would fail until the barclamp's pull request is merged and
packaged.

For now we will focus on getting your barclamp merged, and scale the last
hurdle: getting reviewer approval.

#### Reviewer Approval

Once all automated tests pass (or even before). Members of the Crowbar team
will comment on your code and ask you about the reasons for architectural
decisions or implementation details. They may also ask you to change things. If
that happens you may again have to go back to testing your changed code locally
to verify it still performs as requested. In fact this is strongly recommended
for any change, since `mkcloud` gating will not reveal problems introduced by
these changes without the test automation pull request from the next section.

Once they are happy they will approve your pull request. Your pull request will
need two approvals by Crowbar team members to get merged. Before it gets
merged, please rebase your pull request so it only consists of a single commit.

#### Adding Test Automation

To include your barclamp in `mkcloud` test automation you would fork the
[automation](https://github.com/SUSE-Cloud/automation) repository, add your
barclamp to the list of barclamps enabled by `qa_crowbarsetup.sh` and submit a
pull request. You can use 
[this pull request](https://github.com/SUSE-Cloud/automation/commit/340de0744bae660a1fabbdec812929b9a159a586)
which adds Barbican as an example. Note the default value for the
`want_barbican` variable which is empty in this commit. For the Barbican
barclamp, this was changed in a
[follow-up commit](https://github.com/SUSE-Cloud/automation/commit/0d9d0115fcb8eb236e16effed2e5e5a354fb142a) 
as not all Barbican packages were available at the time of the first commit.

In the simplest case you should be able to combine something analogous to this
commit into two pull request. For less than simple scenarios (e.g. if your
barclamp needs VM side infrastructure such as additional block devices in
place) you will have to add additional setup code to 
[mkcloud](https://github.com/SUSE-Cloud/automation/blob/master/scripts/mkcloud)
and/or
[qa_crowbarsetup.sh](https://github.com/SUSE-Cloud/automation/blob/master/scripts/qa_crobarsetup.sh)
to take care of your barclamp's needs.
