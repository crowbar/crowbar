### Coding and Building Crowbar - DevTool

The DevTool has significant inline documentation.  It is strongly recommended to review the code for help with DevTool!

#### Building

`./dev build --os ubuntu-12.04`

#### Submitting Pull Requests

1. `./dev pull-requests-prep`
1. follow the instructions from the previous step

#### Running Tests

Prerequs
1. Ubuntu 12.04.01 as the OS for DevTool
1. `sudo gem install i18n active_support builder rails`

To run the tests with DevTool, you must first setup the tests using `./dev tests setup`.  DevTool will prompt you to install any missing components.


After you have completed setup, use `./dev tests run` to run the tests.

If you want to troubleshoot the tests, all the files are in `/tmp/crowbar-dev-test/opt/dell/crowbar_framework/`.  You can manually run the DevTool rails application by running `bundle exec rails s` from the dev test directory.
