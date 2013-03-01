### Rails RSpec Testing

Crowbar includes unit tests based on RSpec for more complex unit testing needs.  The [default unit tests](units.md) will be [phased out over time](../testing.md).

The RSpec tests reside under the `spec/` directory.  They should focus on model and non-visible controller tests.  In contrast, [BDD tests](bdd.md) should focus on UI/API controller and view testing.

#### Running the tests via the dev tool

[Running the tests via the dev tool](devtool.md) is the recommended approach.

#### Running the tests outside the dev tool

On an installed admin node, first `cd` to `/opt/dell/crowbar_framework`.
In a [development environment](../dev-vm.md), instead `cd` to `/tmp/crowbar-dev-test/opt/dell/crowbar_framework`.

Then run the following commands:

1. `rake db:drop`
1. `rake railties:install:migrations`
1. `rake db:migrate`
1. `rake spec`

#### Adding new specs

Each barclamp can add tests by dumping them into the `spec/` directory.  The spec tests will pick them up and attempt to run them.
