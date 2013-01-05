### Create Barclamp Stub
**Version 2.x update**: This is current changing with [[Crowbar 2.0]] refactoring.  Documentation may move the the new book-developersguide (link pending)

#### Version 1.x

**15 minute how to video: http://www.youtube.com/watch?v=vSazhFcXd4k**

A barclamp is a deployment module that is imported from its own code repository into the Crowbar framework. A barclamp cannot operate without Crowbar, but you do not have to create a unique build of Crowbar in order to create a barclamp.

The first thing to know about barclamps is that most of the work (80%!) is building your Chef cookbooks. If you don’t have a cookbook that deploys your application then stop here and work on that first.

The second thing to know about barclamps is that there are a lot of them that you can study for examples. Check out the Glance barclamp if you have a single server deployment, Nagios if you have a service that needs to be integrated into every node, Nova if you have a complex multi-component system and Provisioner if you want to impact core Crowbar functionality.

We’ve done a lot of work to make it easy to create and install a stub barclamp. Our experience is that building a barclamp is a highly iterative exercise with a lot of testing. Luckily, Crowbar’s primary mission is to help you brush, rinse and repeat. From there, you can customize and extend your barclamp to deploy your application’s full untamed glory.

Before you try to create a new barclamp, you must install Crowbar.

#### Creating a barclamp

The following steps use the barclamp_model that included under /dell/opt and is described below.

1.     Figure out the name of your barclamp. I’m naming our example “foo barclamp”
   1.         Barclamps must have unique names.
   1.         Do not use spaces or hyphens.
1.     From the Crowbar server, become the super admin: sudo -i
1.     Create a directory for your barclamp: mkdir /barclamps
1.     Run the barclamp create script: /opt/dell/bin/barclamp_create.rb foo “Zehicle” /barclamps
   1.         “foo” is our barclamp name [required]
   1.         “Zehicle” is my company name for the copyright information [default is Dell]
   1.         “/barclamps” is the path where we are putting the barclamp [default is /opt/dell/barclamps]
1.         Result will be a populated barclamp. In this example: /barclamps/foo

That’s it! If you want to plan ahead then you could use an initialized git repo as the target.

Reminder: In building your barclamp, you’ll need to learn about Chef, how Crowbar extends cookbooks and how barclamps interact. That’s beyond the scope of this post.

#### Barclamp layout

A barclamp has the following core components:

>     crowbar.yml configuration file (documented below)
>     README.txt file (optional, recommended)
>     chef directory containing
>         Cookbooks directory with Chef cookbooks
>         Data_bags directory with Crowbar configuration files
>         Roles directory with Chef roles used by the cookbooks and data_bags
>     crowbar_framework directory
>         app directory with Crowbar model, controller, and view code
>         other optional directories to add components needed by the UI such as images

The barclamp_model has a functional layout that covers most configuration requirements. The string ==BC-MODEL== indicates places where the name of the barclamp must be substituted. It is critical to understand that the name of the barclamp is embedded into the barclamp path and file names! This is needed to avoid file collisions when the barclamp is imported.
Crowbar.yml

The crowbar.yml file is a required configuration file that gives direction to the installer. The file has many components and documented on the [[Barclamp crowbar.yml]] page.

The minimum components are:
> barclamp:
>   name: name of your barclamp (required, do not use space or hyphens) 
>
> crowbar:
>
>   layout: 1 (use the # one. This is required because it tells the installer what to expect inside your barclamp)
>

#### Barclamp Proposal

To provide configuration information for your barclamp when handling node transitions or cookbook attributes, a proposal and its validation schema needs to be defined.  The crowbar framework defines a basic schema for all proposals.  This is required to meet basic crowbar functions.  This is versioned and controlled by the installation process to make sure the schema is up to date or updated to the latest.  The base crowbar sections are controlled by the crowbar/proposal_schema_version in the crowbar.yml file.  [[Barclamp Proposal Schema]] description and versions.

The attributes section of the proposal defines the barclamp's unique configuration space.  This is versioned and controlled by the barclamp/proposal_schema_version.  The installer doesn't do anything with this currently.


