# Crowbar and Travis CI [![Build Status](https://travis-ci.org/crowbar/travis-ci-crowbar_framework.png?branch=master)](https://travis-ci.org/crowbar/travis-ci-crowbar_framework)

Crowbar uses Travis CI (among other things) to perform continuous integration.

This is usually pretty easy to setup and Travis CI will automatically run the
tests on every commit. Unfortunately, the current project design prevents this,
because the working Crowbar application must be assembled from several Git
repositories - the main Crowbar framework and its required barclamps.

Hence we use a cron job that runs the `dev` tool every 5 minutes to assemble
the application, and pushes that to a [separate Git repository]
(https://github.com/crowbar/travis-ci-crowbar_framework). This repository is
then [linked to Travis CI]
(https://travis-ci.org/crowbar/travis-ci-crowbar_framework).

## Setup details

The cron job calls the `update-git.sh` script every 5 minutes:

```
 */5 * * * * cd ~/crowbar/travis-ci && ./update-git.sh >>update-git.log 2>&1 
```

This logs all output to `~/crowbar/travis-ci/update-git.log`, which looks like:

```
2013-01-13 01:30:01 +0100: Running ./dev fetch...
2013-01-13 01:31:29 +0100: Running ./dev sync...
2013-01-13 01:31:49 +0100: Running ./dev setup-unit-tests --no-gem-cache
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
(https://github.com/crowbar/crowbar#readme)

## What's next / TODO / Wish list

* Currently only the unit and spec tests are executed. Will try to get the
  Erlang BDD tests running too.
* Move the cron job that executes `update-git.sh` to a proper continuous
  integration server (instead of a VM on my workstation). For example,
  http://ci.opensuse.org/.
* Revert to standard setup once the epic barclamps to Rails Engine refactoring
  is complete.
* Run ChefSpec tests.
* Get the HTML code coverage reports out and put them somewhere public visible.
