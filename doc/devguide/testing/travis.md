# Crowbar and Travis CI [![Build Status](https://travis-ci.org/crowbar/travis-ci-crowbar.png?branch=master)](https://travis-ci.org/crowbar/travis-ci-crowbar) [![Coverage Status](https://coveralls.io/repos/crowbar/travis-ci-crowbar/badge.png?branch=master)](https://coveralls.io/r/crowbar/travis-ci-crowbar) [![Code Climate](https://codeclimate.com/github/crowbar/travis-ci-crowbar.png)](https://codeclimate.com/github/crowbar/travis-ci-crowbar)

Crowbar uses Travis CI (among other things) to perform continuous integration.
The badges above represent (left to right):

  * Travis CI - Are the unit and RSpec tests passing?
  * Coverage status - What % of the code is covered by our tests above?
  * Code Climate - How's the code in terms of complexity and static analysis
    scoring?

Clicking on the each badge will bring you to the respective detailed reports,
which all developers should look at perodically to see where and how we can
improve our code quality. These badges (and others) are also on the [main
project page](../../../#readme).

Test failure notifications are sent to the Crowbar mailing and IRC (#crowbar on
irc.freenode.net).

## Limitations

The current setup works pretty nicely, but there are some limitations due to the
Crowbar architecture:

  * Tests run on batches of squashed commits instead of on every commit. This
    makes it harder to find offending commits.
  * Can't easily test each pull request before they are merged in.

The goal is to ultimately use barclamps like regular gems, then Crowbar can
adopt a standard Rails setup that doesn't have these limitations.

## Setup details

It's usually easy to setup Travis CI, but Crowbar needs to be assembled from
several Git repositories. So we use the [update-git.sh]
(../../../travis-ci/update-git.sh) script to do this and push changes to the
[Travis CI repo](https://github.com/crowbar/travis-ci-crowbar). Each run of the
script looks like this:

```
2013-01-13 01:30:01 +0100: Running ./dev fetch...
2013-01-13 01:31:29 +0100: Running ./dev sync...
2013-01-13 01:31:49 +0100: Running ./dev tests setup --no-gem-cache
2013-01-13 01:32:08 +0100: Copying files...
2013-01-13 01:32:08 +0100: Checking changed files...
2013-01-13 01:32:09 +0100: Committing files...
2013-01-13 01:32:09 +0100: Nothing to commit
```

This is script is run every 5 minutes by our public [Jenkins server]
(https://ci.opensuse.org/job/crowbar-travis_ci-trackupstream/).

### Debugging test failures

You should be able to reproduce all test failures in your local dev environment,
as Travis CI simply launches a fresh Ubuntu VM, sets up the environment and runs
the tests. For example, you can see the [exact commands and output]
(https://travis-ci.org/crowbar/travis-ci-crowbar/jobs/5616682#L1) for each run.
Refer to the [Travis configuration]
(https://github.com/crowbar/travis-ci-crowbar/blob/master/.travis.yml) for
details.

However it's possible that failures may be due to the subtle differences between
the two environments. To more closely mimic the Travis CI environment, you can
do the following:

```bash
git clone git://github.com/crowbar/travis-ci-crowbar.git
cd travis-ci-crowbar/crowbar_framework

# If you have rvm installed, you can optionally create a dedicated
# rvm gemset here:
rvm gemset create crowbar-travis-ci
rvm use @crowbar-travis-ci

# If not, you can add a --path=... parameter to the below to ensure
# that the gems get installed to a clean dedicated environment:
bundle install --without development

# Now run the tests
bundle exec rake db:drop railties:install:migrations db:migrate db:fixtures:dump test:units spec
```

### Setting up the openSUSE Jenkins node

Here's how the openSUSE Jenkins node was setup:

1. Setup the system as described in the [openSUSE dev guide]
   (../dev-vm-SUSE.md).

1. Create a new `crowbar` user account and switch to it.
   ```
   useradd -m crowbar
   su - crowbar
   ```

1. Now ensure you have ssh keys set up for whichever github account you want to
   commit from, then configure git with the corresponding identity (--global is
   required to cover the multiple repositories we are about to clone),
   e.g.:
   ```
   git config --global user.name "Crowbar Travis Bot"
   git config --global user.email "crowbar.gravatar+travis@gmail.com"
   ```

1. Check out and configure the various Crowbar repositories:
   ```
   git clone git://github.com/crowbar/crowbar.git
   cd crowbar
   # GIT_ASKPASS works around a bug in ./dev where it tries to do stuff
   # with Dell-proprietary barclamps.
   GIT_ASKPASS=true ./dev setup --no-github
   ```

1. Check out the `travis-ci-crowbar` repository which the Jenkins job will keep
   updated:
   ```
   cd
   git clone git://github.com/crowbar/travis-ci-crowbar.git
   cd travis-ci-crowbar
   ```

1. Set up a remote from which we can issue pull requests:
   ```
   git remote add personal git@github.com:crowbar-travis/travis-ci-crowbar.git
   ```

1. Perform a dummy push to ensure that the server's ssh key fingerprint
   is in `.ssh/known_hosts`:
   ```
   git push -n personal HEAD
   ```

Now you're done!
