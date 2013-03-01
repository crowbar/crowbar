### Rails Unit Testing

Crowbar includes unit tests based on the `Test::Unit` framework.

The current tests reside under `test/unit/`, and are only focused on model testing.  They use the already present Rails test framework and fixtures under `test/fixtures/`.

#### Running the tests via the dev tool

[Running the tests via the dev tool](devtool.md) is the recommended approach.

#### Running the tests outside the dev tool

On an installed admin node, first `cd` to `/opt/dell/crowbar_framework`.
In a [development environment](../dev-vm.md), instead `cd` to `/tmp/crowbar-dev-test/opt/dell/crowbar_framework`.

Then run the following commands:

1. `rake db:drop`
1. `rake railties:install:migrations`
1. `rake db:migrate`
1. `rake db:fixtures:dump`
1. `rake test:units`

If you want to run just 1 test file, use `rake test:units TEST=test/unit/i18n_test.rb` or similar.

#### Adding new unit tests

It is strongly recommended to write new unit tests using
[RSpec](rspec.md) instead, for reasons listed in the [testing page](../testing.md).
However, each barclamp can add tests by dumping them into the `test/`
directory.  The unit tests will pick them up and attempt to run them.