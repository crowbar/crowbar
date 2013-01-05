### Chef Recipe Unit Tests

Crowbar also includes chefspec for unit testing chef recipes.  This is still a work in progress in the Chef community but has some potential for at least making sure things are doing basic tests.

The current tests reside under spec in each cookbook.

Steps to execute tests on an installed admin node:

1. `cd /opt/dell/chef/cookbooks`
1. `rm README`
1. `rspec *`

Each cookbook can provide their own spec tests.

