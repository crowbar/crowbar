# Barclamp Development Exercises

This guide provides explanation and hands-on exercises to help with
understanding some of the more useful or unintuitive parts of Crowbar
development.

The exercises here will have you modify the files on-disk on the Crowbar admin
node. Your development workflow may involve synchronizing development changes
from your local workstation to the admin node in some other way, this does not
cover that.

For the sake of practicing, these exercises will only concern the keystone
barclamp. This way you only need to deploy up through the keystone barclamp
which will reduce the amount of time it takes to deploy changes.

## Override Config Files

Crowbar leverages the ``oslo.config`` library to support reading configuration
from directories under ``/etc/<service>/<service>.conf.d``. The service package
will install the default config file in ``/etc/<service>/<service>.conf`` and a
small set of overrides in
``/etc/<service>/<service>.conf.d/010-<service>.conf``. Crowbar then applies its
service configuration in ``/etc/<service>/<service>.conf.d/100-<service>.conf``
which overrides all configuration in the default config file or the package's
override config file. Because Chef is an traditional-style configuration
management tool, it attempts to converge a server's configuration by reapplying
recipes regularly, about every 15 minutes or so. This means that any local
changes you make to the ``100-<service>.conf`` files that Crowbar manages will
be eventually overwritten. Using override config files is a useful way of
working around this, since those files will not be overwritten. Override config
files are also useful for handling any parameters not yet supported by Crowbar,
or for temporarily applying a setting without going through a barclamp proposal.

### Exercise

Create a file ``/etc/keystone/keystone.conf.d/200-debug.conf``.

In the file, add the content:

```
[DEFAULT]
debug = true
insecure_debug = true
```

Restart the keystone service:

```
# systemctl restart apache2
```

The ``insecure_debug`` option is now enabled in keystone, which Crowbar doesn't
support. This option is useful for debugging authentication requests on
non-production systems. You can see it in action by requesting tokens from
keystone using invalidate credentials or incorrect scopes.

## Adding a Parameter to a Barclamp

Adding a new parameter involves making changes to several components, but is
ultimately a matter of copying and pasting similar examples. This exercise will
add the ``insecure_debug`` parameter to the keystone barclamp. In reality we
should never support setting this option through Crowbar because it is insecure
for production use, but it is useful as an exercise.

### Exercise

This is done in three stages.

#### Add the parameter to the data bag

Edit the keystone data bag schema in
``/opt/dell/chef/data_bags/crowbar/template-keystone.schema``. Add a new
parameter ``insecure_debug`` under the existing ``debug`` parameter:

```
{
  "type": "map", "required": true,
  "mapping": {
    "id": { "type": "str", "required": true, "pattern": "/^keystone-|^template-keystone$/" },
    "description": { "type": "str", "required": true },
    "attributes": { "type": "map", "required": true,
      "mapping": {
        "keystone": { "type": "map", "required": true,
             "mapping": {
                    "debug": { "type": "bool", "required": true },
                    "insecure_debug": { "type": "bool", "required": true },
...
```
Edit the default data bag in
``/opt/dell/chef/data_bags/crowbar/template-keystone.json``. Add the new
parameter there as well. Here you should also increment the schema revision
number.

```
{
  "id": "template-keystone",
  "description": "Centralized authentication and authorization service for OpenStack",
  "attributes": {
    "keystone": {
      "debug": false,
      "insecure_debug": false,
...
  },
  "deployment": {
    "keystone": {
      "crowbar-revision": 0,
      "crowbar-applied": false,
      "schema-revision": 302,
```

Add a schema migration in ``/opt/dell/chef/data_bags/migrate/keystone/``. The
``300_noop.rb`` migration can be copied for the function prototypes. The file must
start with the new schema revision number that you incremented in the default
data bag and should describe the migration action. For example, if the new
revision number is 302, the schema migration should go in
``/opt/dell/chef/data_bags/crowbar/migrate/keystone/302_add_insecure_debug.rb``. The
upgrade migration needs to take the new parameter from the template attributes
hash and add it to the attributes hash, and the downgrade needs to do the
opposite. It will look something like this:

```
def upgrade(template_attrs, template_deployment, attrs, deployment)
  attrs["insecure_debug"] = template_attrs["insecure_debug"] unless attrs.key?("insecure_debug")
  return attrs, deployment
end

def downgrade(template_attrs, template_deployment, attrs, deployment)
  attrs.delete("insecure_debug") unless template_attrs.key?("insecure_debug")
  return attrs, deployment
end
```

Run the migration. There are two ways to do this:

```
# barclamp_install.rb /opt/dell/crowbar_framework/barclamps
```

will reinstall all the barclamps, including running all the migrations. Don't be
confused by the ``.rb`` file suffix, the command should be an executable already
in your PATH. Alternatively:

```
# cd /opt/dell/crowbar_framework
# RAILS_ENV=production rake crowbar:schema_migrate
# RAILS_ENV=production rake crowbar:schema_migrate_status # optional
```

You can use the built-in rake commands to do the migration.
``crowbar:schema_migrate`` does the migration, ``crowbar:schema_migrate_status``
shows the revision number for each barclamp. You can use ``rake -T`` while in a
directory containing a ``Rakefile`` to see all the available commands. The
default Rails environment is ``development`` but we never configure a
development environment, so you need to specify ``production``.

Once the migration is run, check the raw view of the barclamp in the crowbar UI
to make sure the parameter appears.

#### Add the parameter to the cookbook

The cookbook contains the Ruby code that creates and applies changes to the
deployment.

Edit the keystone server recipe in
``/opt/dell/chef/cookbooks/keystone/recipes/server.rb``. Find the template
resource that creates the keystone config file and add the new parameter to the
list of variables it passes to the template.

Edit the keystone config template in
``/opt/dell/chef/cookbooks/keystone/templates/default/keystone.conf.erb`` to use
the new parameter.

Chef does not pick up changes to cookbooks automatically, they need to be
uploaded to the Chef server. The ``barclamp_install.rb`` tool will take care of
this:

```
# barclamp_install.rb /opt/dell/crowbar_framework/barclamps
```

Alternatively, you can use the Chef tooling directly to upload the cookbook:

```
# knife cookbook upload keystone -o /opt/dell/chef/cookbooks
```

Now you should be able to apply the change to the deployment. In the crowbar UI,
click "Apply". The new parameter should appear in the keystone config file after
the proposal is finished running. If you change the value in the raw view of the
proposal and reapply, the config file will have changed.

#### Add the parameter to the UI

In many cases this is as far as we go, but for some common operations it is good
to expose the parameter in the "Custom" view of the crowbar UI.

Edit the locales file at
``/opt/dell/crowbar_framework/config/locales/keystone/en.yml``. Add the new
parameter to the YAML file under ``edit_attributes``. The name must match the
name given in the data bag. Add descriptive text to the attribute, this is what
will appear in the web UI to the user.

Edit the barclamp view in
``/opt/dell/crowbar_framework/app/views/barclamp/keystone/_edit_attributes.html.haml``
and add a new ``boolean_field`` for the parameter.

When finished changing the Rails code, restart crowbar:

```
# systemctl restart crowbar
```

Refresh the custom view of the barclamp to see the new dropdown menu appear.

## Synchronizing Chef in HA Deployments

When applying a proposal, Crowbar runs Chef in parallel as much as it can. This
means we don't have to wait for each node to complete its configuration in
serial, but it has implications for interdependent resources. For example, when
an OpenStack service is deployed, a database synchronization must be run to set
up tables for the service. This should only be done once, by one actor. If
multiple nodes try to run the db sync at the same time, a race condition will
occur and one will fail due to the database transaction already occurring.
Moreover, the systemd services depend on that db sync happening before they try
to start, so they need some way of being notified that it is done.

This is implemented in crowbar with sync marks. One node in a cluster is chosen
to be the "founder" node that behaves as the instigator for actions that can't
be run parallel. Other nodes are notified to wait for it to complete. In this
exercise you will implement sync marks to prevent race conditions when the
``keystone-manage mapping_populate`` command is run.

See the [training the HA implementation in
Crowbar](https://w3.suse.de/~aspiers/cloud/HA-training/) for more information.

### Exercise

For this exercise, crowbar should be configured in HA mode with at least two
controllers, and LDAP should be configured using ``want_ldap=1`` in your
``mkcloud`` script.

The ``keystone-manage mapping_populate`` command attempts to enhance keystone
runtime performance by generating IDs for all LDAP users in advance. The end
result is that the ``id_mapping`` table in the keystone database is populated.
We would not want to have multiple controllers all running this command because
it could cause a race condition or fail if another controller started the
command first, so we want to ensure that only the founder node runs it.

Edit the keystone server recipe in
``/opt/dell/chef/cookbooks/keystone/recipes/server.rb``. Add an ``execute``
resource that runs ``keystone-manage mapping_populate --domain ldap_users``.
Ensure the resource is wrapped between a ``wait`` sync mark and a ``create``
sync mark. You can look at the ``keystone-manage db_sync`` resource as an
example; we'll address the ``ruby_block`` in the next exercise. Upload the
keystone cookbook and apply the keystone proposal to ensure it runs correctly.
Examine the ``id_mapping`` table of the keystone database to ensure it has some
LDAP users populated in it.

## Understanding Chef Compile and Converge Phases

The Chef DSL looks just like Ruby, but it should not be treated the same as
regular Ruby scripts. Chef operates in a two-pass model: the Chef recipes are
first compiled into a catalog, and then the catalog is applied on the nodes.
It's called ["Compile and Converge"](https://coderanger.net/two-pass/).
Essentially, everything in the recipe is run twice: during the compilation
phase, each resource block is expanded and expressions are evaluated, freezing
them in time as they are inserted into the run list. Then those expanded
resources are executed in the converge phase, and in some cases triggering
execution of other resources via Chef's [notification mechanism
](https://docs.chef.io/resource_common.html#notifications). Misunderstanding
this sequence can lead to bugs in which values are evaluated incorrectly and
resources are misapplied. This exercise will demonstrate when different types of
code are executed and how to avoid common pitfalls.

### Exercise

In some cases, commands executed by Chef cookbooks are not idempotent, or are
idempotent but are time consuming, and therefore it's best to only run them on
the first deployment. Moreover, in HA deployments, we want all nodes in a
cluster to wait for the founder to complete an action, and then not try to
repeat the action. Chef allows us to manage this by setting persistent flags
on the node that we can evaluate before deciding to include a resource in the
Chef run.

To demonstrate the importance of knowing the difference between the compile and
converge phases, let's first do this incorrectly.

First, undo the result of the last successful ``mapping_populate`` run by
emptying the table:

```
# mysql keystone -e 'delete from id_mapping'
```

Also, change the command in the ``execute`` resource you added in the previous
exercise to return an error, for example by changing the command to ``false``.
This will illustrate what happens when the command fails to execute.

After the ``execute`` resource in the recipe, set a flag on the node object to
declare that the mapping population has been run, for example:

```
node.set[:keystone][:mapping_populated] = true
node.save
```

Also add an ``only_if`` parameter to the ``execute`` resource so that it will
only run if that flag is not set. The (naive) idea is that the ``execute``
resource should run once, then the flag should be set, and then on the next Chef
run it won't be executed. Moreover, if the resource fails, we would want
execution to stop so that the problem can be corrected, and then we would want
the next run to try the mapping population again.

Upload the cookbook and reapply the proposal. Notice the proposal does not fail
on the invalid command in the ``execute`` resource. Also check the database on
the controller and notice that nothing happened.

Fix the command in the ``execute`` resource to use the correct mapping populate
command. Upload the cookbook again and reapply the proposal. This should again
be successful, but the command still will have not populated the database.

Why not? Because the code to set the ``:mapping_populated`` flag was run in
the *compile* phase, long before the ``execute`` resource was run. When it came
time to run the resource, it saw that the flag on the node was already set and
decided not to run the resource. You can examine the node attributes with knife:

```
# knife node list
# ...
# knife node edit <controller1>
```

Go ahead and reset the node by deleting that ``"mapping_populated"`` flag in the
node JSON in knife. Do the same on all other controllers.

Now let's do it correctly. In the recipe, wrap the node attribute setting in a
``ruby_block``:

```
ruby_block "mark node for keystone mapping_populate" do
  block do
    node.set[:keystone][:mapping_populated] = true
    node.save
  end
  action :nothing
  subscribes :create, "execute[keystone-manage mapping_populate]", :immediately
end
```

Make sure the ``ruby_block`` comes before the closing sync mark. The
``ruby_block`` is a Chef resource, which means the action it defines won't be
executed until the *converge* phase. Now when you upload the cookbook and apply
the proposal, the mapping population should happen correctly. It should also
occur on only one of the controllers, and it won't occur on any following Chef
runs.
