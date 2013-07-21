This file documents how to build the Crowbar installation DVD image.

Prerequisites:

  * An unfiltered Internet connection and a decent amount of bandwidth.
    The first time you try to build Crowbar it will need to download
    all the packages that it will need to stage Crowbar onto an OS install
    DVD.
    
  * bash version 4 or higher
    
  * mkisofs
  
    The end product of the build script is an ISO image that will be used
    to bootstrap the Crowbar admin node.
  * debootstrap (if staging on to Ubuntu)
    
    The build process needs to download all the .debs and gems that 
    Crowbar requires, and we don't want to inadvertently mess up the build
    machine when we do that.  All extra packages are downloaded into a
    chrooted minimal Ubuntu intall, and we use debootstrap to enable that.

  * git

    If you are reading this locally, you probably already have git
    installed. If not, you will need it to allow the dev tool to
    manage all your barclamps.

  * curl

    The build process may need to download files from the Internet.
    It uses curl to do this.
  * ruby

    Most of Crowbar is written in Ruby, and some of the helper
    programs we use during the build process are also written in Ruby.
    The Crowbar build process works with all Ruby versions from 1.8.7
    to 2.0.0.  In addition to the basic Ruby install, the following
    ruby gems are also required:
    * json (only on Ruby < 2.0)
    * libxml-ruby
    * xml-simple
    * kwalify
    * bundler
    * builder

  * rpm and dpkg

    The build process needs to be able to introspect on package
    dependencies for the packages it will windo up staging on the
    final generated ISO.

  * dpkg-buildpackage and debhelper

    These are used to build native .deb packages for Crowbar 2.0 releases.

  * rpm2cpio

    The Sledgehammer build process downloads raw RPM files to build
    the Sledgehammer system discovery component.  It needs rpm2cpio to
    create its initial chroot to bootstrap its build process.

  * cpio

    We need to extract the cpio files that rpm2cpio creates, so we
    have to have cpio as well.

  * createrepo

    If the Sledgehapper and Crowbar build processes need o create a
    yum or zypper archive, they use createrepo to do this.

  * cabextract

    The dell\_bios barclamp needs to extract its configuration from a
    .cab file downloaded off of ftp.dell.com so that it can find the
    latest firmware components for PowerEdge R series hardware.

  * erlang

    The BDD test suite that the Crowbar unit test framework is written
    in requires Erlang.
    
  * Sudo to root privileges for the following commands:
    * /bin/mount, /bin/umount
      We have to be able to mount and umount the Ubuntu .iso image, as well as
      a tmpfs for debootstrap, and we have to be able to bind mount
      /dev, /dev/pts, /proc, and /sys into the debootstrap chroot environment.
    * /usr/sbin/debootstrap
      debootstrap requires root privileges to run.
    * /bin/cp
      We need to copy things into and out of the debootstrap environment to
      ensure it downloads and caches the right packages.
    * /usr/sbin/chroot
      All our package caching is done in a chroot environment, and chroot
      requires root permissions to run.

      Most of the full-time Crowbar developers just arrange to have
      passwordless sudo rights for their primary development account.

Most builds should be done using the dev tool, which will ensure that
all the barclamps are checked out to the proper branches and that all
the trees are clean before starting the build process.  For example,
to build an Ubuntu 12.04.2 ISO that has just the core Crowbar
components staged on it, you can run the following commands:

        cd <your top-level crowbar checkout>
        ./dev switch development/master
        ./dev build --os ubuntu-12.04

Command Line Parameters:

  * --os
    The argument to this parameter is the OS to stage Crowbar on to.
    Crowbar currently understands how to stage itself on:
    * ubuntu-10.10
    * ubuntu-12.04
    * redhat-5.7 and centos-5.7
    * redhat-6.4 and centos-6.4
    * fedora-18
  * --update-cache
    This parameter forces the build to try and update the build cache again.
    Use this parameter if you want to pull in updates from the upstream
    repositories that Crowbar pulls packages from.
  * --test
    This tells the build system to try and smoketest the freshly-generated
    .ISO. Any arguments after this that do not begin with a hyphen are
    passed to the test framework -- please see test\_framework/README.testing
    for more information.
  * --no-iso
    Do everything but actaully build the iso.
  * --wild-cache
    Create a temporary cache directory that is good for this build
    only. The cache will be deleted when the build exits.  This exists
    mainly to test what a build that is pulling the latest stuff from
    the Internet looks like.  You should make sure that you are using
    a caching web proxy such as polipo or squid, otherwise repeated
    builds will be very slow.
  * --no-switch
    Leave the branches in the barclamp exactly as they are instead of
    trying to ensure that they are on the proper branch for the build.

Customization:

The build process has several different parameters you can tune, either from
$HOME/.build-crowbar.conf (for developer use), or from build-crowbar.conf
in the current directory (for automated builds).

Here are the parameters you can change through the above configuration files:

  * DEBUG
     If DEBUG is set to anything, build\_crowbar will run in debug mode, and will
     print a transcript of everything it is doing to standard error.
     Setting DEBUG will generate a very large about of output, and is
     useful in diagnosing errors in the build process.
  * CACHE\_DIR
     This is the default location where build\_crowbar.sh will keep the files
     it caches, along with the temporary directories used to mount the
     ISO image, the debootstrap chroot, and the directory we perform the build
     in.
     It defaults to $HOME/.crowbar-build-cache.
  * ISO\_LIBRARY
     This is the default location where the ISO we will stage Crowbar
     on to is stored.
     It defaults to $CACHE\_DIR/iso
  * ISO\_DEST
     This is the location that we will save the Crowbar install image to.
     It defaults to the current directory.
  * IMAGE\_DIR
     This is the location that we will mount isos in.
     It defaults to $CACHE\_DIR/image
  * SLEDGEHAMMER\_PXE\_DIR
     This points to the location we expect to find the unpacked Sledgehammer
     PXE boot archive.  It defaults to $CACHE\_DIR/tftpboot
  * VERSION
     The default version of Crowbar.  Defaults to dev.
  * BUILT\_ISO
     The name of the ISO that build\_crowbar.sh generates.
     Defaults to crowbar-$VERSION.iso
  * CROWBAR\_DIR
     The directory that the Crowbar source is cheched out to.
     Defaults to the directory that build\_crowbar.sh is in.
  * VCS\_CLEAN\_CMD
     This is the command that build\_crowbar.sh will run to clean the tree before
     staging the Crowbar build.
     Defaults to 'git clean -f -d'

Build System Walkthrough:

When build\_crowbar.sh is invoked, it performs the following processes in order:

 1. Make sure we are in the C locale, and that $PATH is set to something sane.
 2. Pick up any local configuration settings from $HOME/.build\_crowbar.conf
    or ./build\_crowbar.conf
 3. Set any uninitialized config variables to their defaults.
 4. Source our generic build and test functions.
 5. Figure out what OS we want to stage, and source the build and test
    libraries.  This will pull in the functions we need to actually stage
    Crowbar on an OS install ISO.  If we were asked to build on an OS that 
    we don't have build info for, die and print out the OSes we do know how
    to stage things on.
 6. Make sure that all the commands we will need to stage Crowbar on to an ISO
    are installed on the system.  If they are not, print a helpful error 
    message and die.
 7. Grab the build lock to make sure that multiple builds do not stomp all
    over eachother.
 8. Do a little bookkeeping to make sure we are on a buildable Git branch.
    If the build cache is in a git repository, record that information as well.
 10. Parse our commandline options. 
 11. Make sure our essential build-related directories are present (including
    the directory we will stage the build into, the directory we will mount
    the ISO image on, and a chroot that will be used as part of the barclamp
    staging process), and set up any build parameters that have not already 
    been set up.
 12. If we were not passed a list of barclamps to install on the command line,
    figure out what barclamps we need based on the submodule information from
    the git branch we are on.
 13. Pull in metadata from the crowbar.yml files for each barclamp.
    This metadata will drive the rest of the install -- we need it to
    figure out dependency relations between barclamps, what packages and files
    to stage, and how to invoke any external build processes we might need.
 14. Make sure we have a Sledgehammer image handy, and build it if we don't.
 15. If we don't have the OS ISO to stage on, and we know how to get one, then
    download the .ISO we will need.
 16. Clean out any leftovers from the last build, and make sure that
    we don't inadvertently pull in any VCS cruft.
 17. Mount the OS iso as a loopback file system, and index its package
    pool if we don't already have it cached.
 18. Stage some barclamp-independent build information into the build directory.
 19. Create the build-info file for this build, and start adding useful 
    metadata into it.
 20. Loop over the list of barclamps want to stage, and stage each one.  
    This is covered in more detail in the Barclamp Staging Walkthrough below.
 21. Bundle each barclamp and its package cache into a per-barclamp tarball.
 22. Create some legacy symlinks, and stage any custom proposals that this
    iso will use.
 23. Perform any OS specific fixups that are needed to make this image deploy
    correctly.
 24. Stage the Sledgehammer image.
 25. Create the Crowbar .ISO by merging the contents of the build and the image
    directory.  Wherever there is a conflict in file names or contents, the
    build directory has priority.  If we were asked to generate a shrunken ISO,
    that happens here.
 26. If we were asked to test the ISO, invoke the test framework on our 
    newly-created ISO.
 27. Clean up after ourselves.

Barclamp Staging Walkthrough:

build\_crowbar will try to stage each barclamp in dependency order (as inferred
from each barclamp's crowbar.yml file).  Staging a barclamp properly for the
OS we are staging on to requires the use of a chroot environment to ensure
that we get all the packages we need and that we don't break the host OS
in the process.  Each barclamp is staged in 6 phases:

  1. Check to see if all the OS packages listed in the pkgs: and build\_pkgs: 
   section of the crowbar.yml are present in this barclamps's OS build cache.
   If they are not, fetch any missing ones and all their dependencies using
   a chroot environment, and add any new or updated packages so fetched back
   into the build cache.
  2. Check to see that all the gems listed in the crowbar.yml are present in
   the build cache.  If they are not, fetch them and all their dependencies in
   the chroot environment, and add any new or updated gems to the build cache.
  3. Download and cache any packages pointed to by raw\_pkg stanzas in the 
   crowbar.yml that we are missing.
  4. Download and cache any files required by extra\_files stanzas in the
   crowbar.yml thatwe are missing.
  5. If the crowbar.yml has a build\_cmd stanza, source that file and use it
   to build an external package.  The script pointed to by build\_cmd should
   have two functions declared:
   
   bc\_needs\_build -- This function should return 0 if the external
   pacakge needs building, and 1 if it does not.
   
   bc\_build: This function will be invoked after setting up a chroot and
   bind-mounting the build cache for this barclamp into it.  It is responsible
   for using the chroot enviromnent to build the external package, and making
   sure that the output of the build process winds up in the proper location
   in the build cache so that the rest of the barclamp can properly use it.

   bc-build has access to the following environment variables:
   BC\_DIR = the full path to the root of the barclamp source repository.  
   BC\_CACHE = The full path to the barclamp package cache.

   Any actual building should happen in the chroot environment. 
   To facilitate this, $BC\_CACHE is bind-mounted to /mnt in the chroot,
   any build\_pkgs required by this barclamp will be installed in the chroot,
   and /mnt/current\_os in the chroot will be a symlink to the OS package cache
   that for the barclamp build cache that is bind mounted to /mnt in the chroot.

   You can use the chroot\_install command to install any additional packages
   you may need, and you can use the in\_chroot command to run commands
   in hte chroot environment.

   For some in-tree examples, refer to the ganglia, provisioner, and deployer
   barclamps.  All of these have an additional script that is copied into the
   chroot that handles most of the build tasks.

  6. Tar up the source for the barclamp and the build cache into a deployable
   barclamp tarball.

The build\_crowbar.sh script is heavily commented, please refer to it for more 
detailed information.
