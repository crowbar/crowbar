## Making a Barclamps work with Crowbar

This section shows you how to make barclamps.

### Customization Locations

To create a barclamp, you must update many places with the name of your barclamp.  While this seems redundant inside the barclamp, it is essential when your barclamp gets merged into the larger Crowbar framework.  This often means that your barclamp tree will have one or two files inside a [barclamp] subdirectory.  That's normal and you'll be glad of it when you have to troubleshoot your barclamp later.

Locations of Crowbar Components in a typical barclamp

* `/bin` 
* `/chef` 
   * `/`
* `/crowbar_framework` 
   * `/app` - contains the code for the barclamp.  Crowbar does not require much code for basic operations, but stubs are required
      * `/controllers`
      * `/models`
      * `/views`
        * `/barclamp/[barclamp]/*.haml` - attribute & deployment view pages
      * `/asset`
   * `/BDD` - integrated testing framework (see DevGuide Testing)
   * `/doc` - integrated documentation frameowkr (see DevGuide documentation) 
      * `[barclamp].yml` - index of your barclamps additions to the documentation
      * `/default/[topic].md` - documentation files (can be in subdirectories)
   * `/db`   
      * `migrate` - required to create the barclamp record so Crowbar can find your barclamp in the database
        * `YYYYMMDDHHMMSS_barclamp_import_[barclamp].rb` - does the migration (see DevGuide barclamp meta data)
      
> Note: The good news is that that barclamp_model will start you off with all of these files populated!

#### Requested Sections
The following items are desired but not yet created in the documenation.

* Creating UI screens
* Adding Chef cookbooks
* ...

#### Create the WebApp Pieces

As a Rails app, you will need to following Rails naming conventions.  Say you name a barclamp, fred_rocks.  This means that the camelized name would be FredRocks.  

#### Create the WebApp Controller

The controller file lives in crowbar_framework/app/controllers.  It should be named bcname_controller.rb.  bcname is the lower case name of with the barclamp.  The class name should be the camelized name of the barclamp with Controller and it should inherit from BarclampController.

It should look like this:
class FredRocksController < BarclampController
end

For normal operation and to provide the default APIs, nothing else need to be provided.

#### Create the WebApp Service Object

The service object lives in the crowbar_framework/app/models directory.  It is usually named bcname_service.rb.  bcname is the lowercase version of the name.  The class name should be the camelized version of the barclamp name with Service and it should inherit from ServiceObject.

It should look like this:
class FredRocksService < ServiceObject
end

For normal operation and to provide the default APIs, nothing else need to be provided.

Within this class, the following routines can be overridden:
* create_proposal(name)
* transition(name, inst, state)
* apply_role_pre_chef_call(old_config, new_config, all_nodes)
* proposal_dependencies(proposal_config)
* validate_proposal_elements(proposal_elements)
* validate_proposal(proposal)

These can be overriden, but usually aren't.  They represent the basic API calls that have complex actions within the barclamp.
* apply_role(new_proposal_config, in_queue)
* destroy_active(prop_name)
* dequeue_proposal(prop_name)
* proposal_create(params)
* proposal_edit(params)
* proposal_delete(prop_name)
* proposal_commit(prop_name, in_queue)

#### Creating Barclamp Views

Crowbar has pre-wired routes that allow barclamps to create custom view pages without changing the routes file.  These views must conform to the following pattern in the Barclamp Controller object.

<table>
  <tr><th>Method</th><th>Route</th><th>Path</th></tr>
  <tr>
    <td>node</td>
    <td>barclamp_node_path(:controller=>_)</td>
    <td>/barclamp/[:controller]/node/[:id]</td></tr>
  <tr>
    <td>network</td>
    <td>barclamp_network_path(:controller=>_)</td>
    <td>/barclamp/[:controller]/network/[:id]</td></tr>
  <tr>
    <td>util</td>
    <td>barclamp_util_path(:controller=>_)</td>
    <td>/barclamp/[:controller]/util/[:id]</td></tr>
</table>

> All these methods can optionally take :id as long as :id is limited to valid ID/Names

> Please review the 'adding-navigation` DevGuide topic for information about including the view in the menu.

#### Importing a barclamp

_this is the manual process, there is a Utility for this in the UI_

Once you created a barclamp, you can import the barclamp into Crowbar & Chef.

Assuming that you already created the foo barclamp in /barclamps, here are the steps:

1.     From the Crowbar server, become the super admin: sudo –i
1.     Run the barclamp install script: /opt/dell/bin/barclamp_install /barclamps/foo
   1.         “/barclamps/foo” is the path to your barclamp. If could be anything!
   1.         The core barclamps are in /opt/dell/barclamps.
   1.         In a vm, you could mount a shared folder to access the barclamp (e.g.: /mnt/hgfs/barclamps)

Your barclamp should now show up in the Crowbar UI! You can also see it in Chef under the Crowbar databag.

While barclamps are generally safe to install multiple times, you can uninstall a barclamp using “barclamp_uninstall.rb /path/to/barclamp”
