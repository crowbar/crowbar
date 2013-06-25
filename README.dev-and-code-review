Dev leverages the Github pull request mechanism to enable handling some
aspects of code review and merging. We do not use pull requests
directly because we are in the habit of making synchronized changes
across barclamps to handle code refactors and updates, and Github does
not have the concept of a pull request touching multiple repositories.

Singleton pull requests and pull request bundles:

 * A singleton pull request is what you get when you submit a pull
   request using the usual Github pull request mechanism.

 * A pull request bundle is what you get when you issue pull requests
   using ./dev pull-requests gen.  Pull request bundles are implemented
   using singleton pull requests + a unique ID for the bundle (currently
   the SHA1 of that SHAs of the tips of all the branches involved), what
   release the pull request bundle is against, and a count of all the 
   pull requests in the bundle.  Each pull request has this information
   added by ./dev pull-requests gen.

Dev finds out what pull requests are available at Github by one of two
ways:

 * ./dev fetch will silently pull down information about open pull
   requests from each repository it touches on Github.
 * ./dev pull-requests fetch <remote> will download information about
   open pull requests from all known Crowbar repositories at <remote>,
   assuming that remote is hosted on Github.

Either way, fetching new pull request metadata goes through the same
set of steps:
 1: Delete all locally cached pull request metadata, including
    tracking branches created to point at the contents of a pull
    request.
 2: Pull down information about open pull requests for each repository
    we are touching as part of the fetch process, creating new
    tracking branches on the fly.
 3: Determine which pull requests are part of a bundle, and
    sanity-check the bundle to ensure that all of the components of the
    bundle are present and consistent with the other components.
 4: If the pull request is not part of a bundle, infer which release
    it applies to based on the branch that it should be merged into.
 5: Sort each singleton and bundle by last modified date, and assign a
    numeric ID to each pull request singleton or bundle.  This bundle
    ID will not change until the next fetch operation.
 5: Print out a status line that says how many singletons and bundles
    are open.

Here is what this looks like:
victor@system76:~/crowbar$ ./dev pull-requests fetch
2012-11-21 10:23:13 -0600: Fetching open pull requests for barclamp ApacheHadoop
2012-11-21 10:23:13 -0600: Fetching open pull requests for barclamp chef
2012-11-21 10:23:13 -0600: Fetching open pull requests for barclamp cinder
2012-11-21 10:23:15 -0600: Fetching open pull requests for barclamp clouderamanager
2012-11-21 10:23:15 -0600: Fetching open pull requests for barclamp crowbar
2012-11-21 10:23:16 -0600: Fetching open pull requests for barclamp database
2012-11-21 10:23:16 -0600: Fetching open pull requests for barclamp dell_bios
2012-11-21 10:23:16 -0600: Fetching open pull requests for barclamp dell_branding
2012-11-21 10:23:17 -0600: Fetching open pull requests for barclamp dell_raid
2012-11-21 10:23:17 -0600: Fetching open pull requests for barclamp deployer
2012-11-21 10:23:17 -0600: Fetching open pull requests for barclamp dns
2012-11-21 10:23:18 -0600: Fetching open pull requests for barclamp ganglia
2012-11-21 10:23:18 -0600: Fetching open pull requests for barclamp git
2012-11-21 10:23:23 -0600: Fetching open pull requests for barclamp glance
2012-11-21 10:23:24 -0600: Fetching open pull requests for barclamp hadoop
2012-11-21 10:23:25 -0600: Fetching open pull requests for barclamp hive
2012-11-21 10:23:25 -0600: Fetching open pull requests for barclamp indigo
2012-11-21 10:23:25 -0600: Fetching open pull requests for barclamp ipmi
2012-11-21 10:23:26 -0600: Fetching open pull requests for barclamp keystone
2012-11-21 10:23:30 -0600: Fetching open pull requests for barclamp kong
2012-11-21 10:23:32 -0600: Fetching open pull requests for barclamp logging
2012-11-21 10:23:33 -0600: Fetching open pull requests for barclamp mysql
2012-11-21 10:23:34 -0600: Fetching open pull requests for barclamp nagios
2012-11-21 10:23:35 -0600: Fetching open pull requests for barclamp network
2012-11-21 10:23:36 -0600: Fetching open pull requests for barclamp nova
2012-11-21 10:23:41 -0600: Fetching open pull requests for barclamp nova_dashboard
2012-11-21 10:23:41 -0600: Fetching open pull requests for barclamp ntp
2012-11-21 10:23:41 -0600: Fetching open pull requests for barclamp openstack
2012-11-21 10:23:41 -0600: Fetching open pull requests for barclamp overview
2012-11-21 10:23:42 -0600: Fetching open pull requests for barclamp pig
2012-11-21 10:23:42 -0600: Fetching open pull requests for barclamp postgresql
2012-11-21 10:23:42 -0600: Fetching open pull requests for barclamp provisioner
2012-11-21 10:23:45 -0600: Fetching open pull requests for barclamp quantum
2012-11-21 10:23:45 -0600: Fetching open pull requests for barclamp redhat-install
2012-11-21 10:23:47 -0600: Fetching open pull requests for barclamp sqoop
2012-11-21 10:23:48 -0600: Fetching open pull requests for barclamp swift
2012-11-21 10:23:48 -0600: Fetching open pull requests for barclamp tempest
2012-11-21 10:23:50 -0600: Fetching open pull requests for barclamp test
2012-11-21 10:23:50 -0600: Fetching open pull requests for barclamp ubuntu-install
2012-11-21 10:23:51 -0600: Fetching open pull requests for barclamp zookeeper
2012-11-21 10:23:53 -0600: Fetching open pull requests for Crowbar
2012-11-21 10:23:55 -0600: 1 open pull request bundles and 13 open singleton pull requests
victor@system76:~/crowbar$

Once pull request data has been cached locally, you can list open pull
requests and bundles:

victor@system76:~/crowbar$ ./dev pull-requests list
1: (development) RHEL OS / MBR install onto first smallest drive
2: (fred) Optimize load time of update nodes recipe
3: (fred) Nova changes for bigger network
4: (development) make it more portable
5: (fred) nagios optimizations
6: (feature/pfs-folsom) updating feature/pfs-folsom/master
7: (feature/pfs-folsom) updating feature/pfs-folsom/master with the latest code
8: (development) Improving definitions documentation; adding minor fix for the Swift PFS support
9: (development) Don't merge me, testing dev tool changes.
10: (betty) Fixing tempest failures when running on Betty release
11: (fred) Fixing tempest failures when running on Fred release
12: (fred) Fixing tempest failures when running on Fred release
13: (development) Adding ability to install Cinder from packages
14: (feature/pfs-folsom) updating feature/pfs-folsom/master
victor@system76:~/crowbar$

Each line consists of the local ID of the pull request, the release
the pull request applies to, and the title of the pull request.

You can also get more specific information on a pull request:

victor@system76:~/crowbar$ ./dev pull-requests show 1
Title:               RHEL OS / MBR install onto first smallest drive
Release:             development
Last Updated:        2012-02-02T20:02:03Z
Type:                singleton
Repo:                barclamp-provisioner
Repo with Changes:   https://github.com/bpezan/barclamp-provisioner.git
Branch with Changes: master
Local Branch:        pull-req-6
Target Branch:       master
Pull Request URL:    https://github.com/crowbar/barclamp-provisioner/pull/6
Builds:              master
victor@system76:~/crowbar$ 

From this, we can see that the oldest pull request (pull requests are
ordered from oldest to newest) is a singleton pull request, was last
touched on 2012-02-02, applies to the provisioner barclamp, and
targets the master build of the development release.

Let's try switching to it:

victor@system76:~/crowbar$ ./dev pull-requests switch 1 master
2012-11-21 10:35:31 -0600: Switched to development/master
Auto-merging chef/cookbooks/provisioner/templates/default/compute.ks.erb
CONFLICT (content): Merge conflict in chef/cookbooks/provisioner/templates/default/compute.ks.erb
Automatic merge failed; fix conflicts and then commit the result.
2012-11-21 10:35:31 -0600: Could not merge pull-req-6 into master in barclamp-provisioner
2012-11-21 10:35:32 -0600: Switching provisioner to master
2012-11-21 10:35:32 -0600: Switched to development/master
2012-11-21 10:35:32 -0600: Pull request 1 does not merge cleanly.  It should be updated or closed.
victor@system76:~/crowbar$ 

OK, this pull request does not apply cleanly to the current set of
trees.  In this case, that is because the provisioner barclamp has
undergone some major reorganization since this pull request was
created, and it is no longer applicable in its current form.  Dev went
ahead and switched all the trees back to a known-good state, which
automatically cleans up.

Let's try a different pull request:

victor@system76:~/crowbar$ ./dev pull-requests show 9
Title:        Don'y merge me, testing dev tool changes. 
Release:      development
Last Updated: 2012-11-19T19:12:46Z
Type:         bundle
Bundle ID:    311c3511f0c9de379cb2b755503b5bc95953352d
Repo: barclamp-keystone
    Repo with Changes:   https://github.com/VictorLowther/barclamp-keystone.git
    Branch with Changes: pull-req-master-311c3511f0c9de379cb2b755503b5bc95953352d
    Local Branch:        pull-req-47
    Target Branch:       master
    Pull Request URL:    https://github.com/crowbar/barclamp-keystone/pull/47
    Builds:              openstack-os-build
Repo: barclamp-mysql
    Repo with Changes:   https://github.com/VictorLowther/barclamp-mysql.git
    Branch with Changes: pull-req-master-311c3511f0c9de379cb2b755503b5bc95953352d
    Local Branch:        pull-req-25
    Target Branch:       master
    Pull Request URL:    https://github.com/crowbar/barclamp-mysql/pull/25
    Builds:              openstack-os-build
Repo: barclamp-network
    Repo with Changes:   https://github.com/VictorLowther/barclamp-network.git
    Branch with Changes: pull-req-master-311c3511f0c9de379cb2b755503b5bc95953352d
    Local Branch:        pull-req-100
    Target Branch:       master
    Pull Request URL:    https://github.com/crowbar/barclamp-network/pull/100
    Builds:              master
Repo: barclamp-provisioner
    Repo with Changes:   https://github.com/VictorLowther/barclamp-provisioner.git
    Branch with Changes: pull-req-master-311c3511f0c9de379cb2b755503b5bc95953352d
    Local Branch:        pull-req-84
    Target Branch:       master
    Pull Request URL:    https://github.com/crowbar/barclamp-provisioner/pull/84
    Builds:              master
victor@system76:~/crowbar$ 

OK, this is actually a pull request bundle that consists of changes to
4 separate barclamps in the development release.  Let's switch to it:

victor@system76:~/crowbar$ ./dev pull-requests switch 9
2012-11-21 10:55:06 -0600: You must pass a build to pull-requests switch, because there is no obvious default.
2012-11-21 10:55:06 -0600: Run ./dev pull-requests builds 9 to see what is available.
victor@system76:~/crowbar$ ./dev pull-requests builds 9
master
openstack-os-build
victor@system76:~/crowbar$ 

Let's try that again while specifying that we want openstack-os-build:

victor@system76:~/crowbar$ ./dev pull-requests switch 9 openstack-os-build
2012-11-21 10:56:47 -0600: Switching chef to master
2012-11-21 10:56:47 -0600: Switching crowbar to master
2012-11-21 10:56:47 -0600: Switching deployer to master
2012-11-21 10:56:47 -0600: Switching dns to master
2012-11-21 10:56:47 -0600: Switching ganglia to master
2012-11-21 10:56:47 -0600: Switching glance to master
2012-11-21 10:56:47 -0600: Switching ipmi to master
2012-11-21 10:56:47 -0600: Switching keystone to master
2012-11-21 10:56:47 -0600: Switching logging to master
2012-11-21 10:56:47 -0600: Switching mysql to master
2012-11-21 10:56:48 -0600: Switching nagios to master
2012-11-21 10:56:48 -0600: Switching network to master
2012-11-21 10:56:48 -0600: Switching nova to master
2012-11-21 10:56:48 -0600: Switching nova_dashboard to master
2012-11-21 10:56:48 -0600: Switching ntp to master
2012-11-21 10:56:48 -0600: Switching openstack to master
2012-11-21 10:56:48 -0600: Switching swift to master
2012-11-21 10:56:48 -0600: Switching tempest to master
2012-11-21 10:56:48 -0600: Switching test to master
2012-11-21 10:56:48 -0600: Switched to development/openstack-os-build
Updating fab5803..a736082
Fast-forward
 crowbar_framework/app/models/keystone_service.rb |   42 +++++++++-------------
 1 file changed, 16 insertions(+), 26 deletions(-)
Updating 01694b9..3fdd8ef
Fast-forward
 crowbar_framework/app/models/mysql_service.rb |   52 ++++++++++---------------
 1 file changed, 20 insertions(+), 32 deletions(-)
Updating 8928c1d..c37ade1
Fast-forward
 chef/cookbooks/network/recipes/default.rb |    3 ++-
 1 file changed, 2 insertions(+), 1 deletion(-)
Updating 98fd84f..d04621f
Fast-forward
 build_curl.sh        |   16 ----------------
 build_curl_chroot.sh |   16 ----------------
 crowbar.yml          |    2 --
 3 files changed, 34 deletions(-)
 delete mode 100755 build_curl.sh
 delete mode 100755 build_curl_chroot.sh
2012-11-21 10:56:48 -0600: You are now on build openstack-os-build in pull request 9.
victor@system76:~/crowbar$ 

That worked.  Let's check out the state of the provisioner barclamp,
which is part of the pull request bundle:

victor@system76:~/crowbar$ cd barclamps/provisioner/
victor@system76:~/crowbar/barclamps/provisioner$ git status
# Not currently on any branch.
nothing to commit (working directory clean)
victor@system76:~/crowbar/barclamps/provisioner$ 

From here, we can see that the provisioner barclamp is not currently
on a branch.  This is by design -- in order to preserve your current
branches and make sure that doing reviews of pull requests is not
destructive, all merges of code from a pull request happen on raw
commits, not branches.  This makes restoring state after examining
pull requests simpler, as well -- since we did not change the state of
any branches, we don't have to worry about changing them back.

From here, we can run unit tests:

victor@system76:~/crowbar$ ./dev tests clear
victor@system76:~/crowbar$ ./dev tests run
... lots of output ...
victor@system76:~/crowbar$

spin up a build (using the --no-switch flag to leave the barclamps
alone) and smoketest it:
victor@system76:~/crowbar$ ./dev build --os ubuntu-12.04 --no-switch --test use-screen tempest
... even more output snipped ...
victor@system76:~/crowbar$

Once you are finshed, dev switch will switch back to the proper
branches for the release/build combo you were on:

victor@system76:~/crowbar$ ./dev switch
2012-11-21 11:17:00 -0600: Switching keystone to master
2012-11-21 11:17:00 -0600: Switching mysql to master
2012-11-21 11:17:00 -0600: Switching network to master
2012-11-21 11:17:00 -0600: Switching provisioner to master
2012-11-21 11:17:00 -0600: Switched to development/openstack-os-build
victor@system76:~/crowbar$

Assuming you liked what you saw and the code passed the test, you can
merge the changes in dev:

victor@system76:~/crowbar$ ./dev pull-requests merge 9
2012-12-05 09:47:25 -0600: https://github.com/crowbar/barclamp-crowbar/pull/287 merged.
2012-12-05 09:47:26 -0600: https://github.com/crowbar/barclamp-glance/pull/58 merged.
2012-12-05 09:47:29 -0600: https://github.com/crowbar/barclamp-hive/pull/19 merged.
2012-12-05 09:47:32 -0600: https://github.com/crowbar/barclamp-keystone/pull/48 merged.
2012-12-05 09:47:34 -0600: https://github.com/crowbar/barclamp-nova/pull/100 merged.
2012-12-05 09:47:38 -0600: https://github.com/crowbar/barclamp-nova_dashboard/pull/43 merged.
2012-12-05 09:47:40 -0600: https://github.com/crowbar/barclamp-openstack/pull/19 merged.
2012-12-05 09:47:41 -0600: https://github.com/crowbar/barclamp-pig/pull/19 merged.
2012-12-05 09:47:46 -0600: https://github.com/crowbar/barclamp-sqoop/pull/20 merged.
2012-12-05 09:47:48 -0600: https://github.com/crowbar/barclamp-swift/pull/40 merged.
2012-12-05 09:47:50 -0600: https://github.com/crowbar/barclamp-tempest/pull/14 merged.
2012-12-05 09:47:52 -0600: https://github.com/crowbar/barclamp-zookeeper/pull/11 merged.
2012-12-05 09:47:52 -0600: Pull requests merged.
2012-12-05 09:47:52 -0600: You can use ./dev fetch to pull in updated pull request information.
victor@system76:~/crowbar$

More stuff on the way, including integrating this with Jenkins and
adding pull request commenting functionality to Dev.

Sample workflow for pull request review:

 1: Reviewer runs ./dev fetch, and notices that there are open pull requests.
 2: Reviewer runs ./dev pull-requests list to see what all pull
    requests are available to be reviewed.
 3: Reviewer runs ./dev pull-requests show <id> to see details on what
    was included as part of the pull request.  This will include links
    to the pull request on Github.
 4: Reviewer uses Github and standard Git tools to compare the pull
    request with the current state of the trees.
 5: Reviewer runs ./dev pull-requests builds <id> to see what all
    builds are applicable to the pull request.
 6: Reviewer tests to see if the build merges cleanly with their
    current code by running ./dev pull-requests switch <id> <build>.
    If there are any merge conflicts, dev will inform the reviewer and
    reset the working trees to their previous state.  Otherwise, the
    working trees will be left in a state that reflects the results of
    merging the pull request into the appropriate release/build in the
    reviewers repository.  Any repositories that have code merged in
    from the pull request will be left in a detached HEAD state, and
    neither the local tracking branches for the pull request nor the
    reviewers branches will be changed.
 7: Reviewer runs unit tests against the merged code.
 8: Reviewer spins up a build and smoketests it.
 9: If the unit tests and functional tests pass, the code passes code
    review, and the submitter has signed a CLA, otherwise the reviewer
    leaves notes in the pull requests that reflect what the submitter
    needs to do to make the pull request acceptable.
10: Reviewer runs ./dev pull-requests merge <id> to merge the pull requests.
