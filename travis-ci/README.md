# Crowbar and Travis CI [![Build Status](https://travis-ci.org/crowbar/travis-ci-crowbar.png?branch=master)](https://travis-ci.org/crowbar/travis-ci-crowbar) [![Coverage Status](https://coveralls.io/repos/crowbar/travis-ci-crowbar/badge.png?branch=master)](https://coveralls.io/r/crowbar/travis-ci-crowbar) [![Code Climate](https://codeclimate.com/github/crowbar/travis-ci-crowbar.png)](https://codeclimate.com/github/crowbar/travis-ci-crowbar)

Crowbar uses Travis CI (among other things) to perform continuous integration.
The badges above represent (left to right):
   * Travis CI - are the unit test passing
   * Coverage status - what % of the code is covered by the units above
   * Code Climate - how's the code doing in terms of complexity and static analysis scoring

This is usually pretty easy to setup and Travis CI will automatically run the
tests on every commit. Unfortunately, the current project design prevents this,
because the working Crowbar application must be assembled from several Git
repositories - the main Crowbar framework and its required barclamps.

Hence we use a cron job that runs the `dev` tool every 5 minutes to assemble
the application, and pushes that to a [separate Git repository]
(https://github.com/crowbar/travis-ci-crowbar) which is
[linked to Travis CI](https://travis-ci.org/crowbar/travis-ci-crowbar).

The application is assembled from a subset of the tree which './dev
tests setup' creates in `/tmp/crowbar-dev-test`.

## Setup details

The cron job calls the `update-git.sh` script every 5 minutes:

```
 */5 * * * * cd ~/crowbar/travis-ci && ./update-git.sh >>update-git.log 2>&1 
```

This logs all output to `~/crowbar/travis-ci/update-git.log`, which looks like:

```
2013-01-13 01:30:01 +0100: Running ./dev fetch...
2013-01-13 01:31:29 +0100: Running ./dev sync...
2013-01-13 01:31:49 +0100: Running ./dev tests setup --no-gem-cache
2013-01-13 01:32:08 +0100: Copying files...
2013-01-13 01:32:08 +0100: Checking changed files...
2013-01-13 01:32:09 +0100: Committing files...
2013-01-13 01:32:09 +0100: Nothing to commit
```

The last line returns the git commit hash if there are changes, and
automatically pushes to new commit to Github. The script accepts a `--no-push`
option which is useful when testing changes to the setup or script.

## Limitations

While it is great to have public continuous integration, there are a couple of
issues with the current setup:

* Tests run on batches of commits (5 minute groups due to the cron job),
  instead of on every commit. This makes it potentially harder to find the
  commit that broke the tests.
* Travis CI's killer feature is the ability to automatically test every open
  pull request, so one can see if it breaks the tests before merging it. This
  doesn't work with our current setup.

We will have to live with these limitations until the refactoring of barclamps
to use Rails Engines is complete. The Crowbar framework Git repository and
each barclamp repository will then be fully unit testable on their own.

## Notifications

Test pass/fail notifications are sent to our IRC channel (#crowbar on
irc.freenode.net). There's also a build status image in the [main README]
(https://github.com/crowbar/crowbar#readme).

## How to set up an openSUSE 12.2 node to run this job

First install the dependencies listed in [the instructions for setting
up an openSUSE development environment](../doc/devguide/openSUSE-dev-env.md).

Create a new `crowbar` user account and switch to it.

    useradd -m crowbar
    su - crowbar

Now ensure you have ssh keys set up for whichever github account
you want to commit from, then configure git with the corresponding
identity (--global is required to cover the multiple repositories
we are about to clone), e.g.:

    git config --global user.name "Crowbar Travis Bot"
    git config --global user.email "crowbar.gravatar+travis@gmail.com"

Check out and configure the various Crowbar repositories:

    git clone git://github.com/crowbar/crowbar.git
    cd crowbar
    # GIT_ASKPASS works around a bug in ./dev where it tries to do stuff
    # with Dell-proprietary barclamps.
    GIT_ASKPASS=true ./dev setup --no-github

Check out the `travis-ci-crowbar` repository which the Jenkins job
will keep updated:

    cd
    git clone git://github.com/crowbar/travis-ci-crowbar.git
    cd travis-ci-crowbar

Set up a remote from which we can issue pull requests:

    git remote add personal git@github.com:crowbar-travis/travis-ci-crowbar.git

Perform a dummy push to ensure that the server's ssh key fingerprint
is in `.ssh/known_hosts`:

    git push -n personal HEAD
