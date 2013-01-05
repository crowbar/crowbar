### Rails Unit Testing

Crowbar includes rails tests.  These tests us the already present rails test framework and fixtures.  

The current tests reside under test.  Unit tests can be added under unit and currently are only focused on model testing.

Steps to execute tests on an installed admin node:

1. `cd /opt/dell/crowbar_framework`
1. `rake db:drop`
1. `rake db:migrate`
1. `rake db:fixtures:dump`
1. `rake test:units`

Each barclamp can add tests by dumping them into the test directory.  The unit tests will pick them up and attempt to run them.

> If you want to run just 1 test file, use `rake test:units TEST=test/unit/i18n_test.rb` or similar