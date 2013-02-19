------------------------------------------------
README for Kiwi source from SUSE Studio
------------------------------------------------

This tarball contains the Kiwi configuration, theming and overlay files that
were exported from SUSE Studio (http://susestudio.com). It allows you to
create your SUSE Studio configured appliance on your local system.

The user is expected to be familiar with using Kiwi. Details on Kiwi can be
found at http://opensuse.github.com/kiwi.

We recommend installing the version of Kiwi found in the openSUSE
Virtualization:Appliances repository, eg:

  http://download.opensuse.org/repositories/Virtualization:/Appliances/<distro_version>

Please send all your questions and feedback to feedback@susestudio.com.


----------------------
Directory structure
----------------------
 source/             - Contains the kiwi config and overlay files for the
                       image.
 README              - This file.
 create_appliance.sh - Helper script to create your appliance.

The create_appliance.sh script simplifies the applicance creation by
automatically running the kiwi prepare and create steps.

It also prompts users to enter the URLs for internal repositories used by the
appliance (eg. SUSE Linux Enterprise repositories) and updates the file
source/config.xml accordingly. A backup copy is made to config.xml.bak before
modifications.

The script warns the user if the installed kiwi version does not match the
expected version. Minor version differences should be tolerable.


----------------------
Creating your appliance
----------------------
Simply run the create_appliance.sh script as root, eg:

  sudo ./create_appliance.sh


If your appliance uses internal (non-public) repositories, you will be prompted
to specify the repo URL before continuing, eg:

  > sudo ./create_appliance.sh
  ** Checking for internal repositories...
  Enter repository URL for 'SLES 11 i386': http://my.repository/path/to/repo


A successful run looks something like this:

  # sudo ./create_appliance.sh
  ** Checking for internal repositories...
  ** Creating appliance...
  /usr/bin/linux32 /usr/sbin/kiwi --build source/ -d image
  May-22 21:01:40 <1> : Destination: /James_JeOS_32bit.i686-0.0.1.oem-kiwi_src/image/build doesn't exist
  May-22 21:01:40 <1> : Description provides no MD5 hash, check
  May-22 21:01:40 <1> : Reading image description [Prepare]...
  ...

  ** Appliance created successfully! (image/James_JeOS_32bit.i686-0.0.1.raw)
  To boot the image using qemu-kvm, run the following command:
    qemu-kvm -snapshot -m 512 image/James_JeOS_32bit.i686-0.0.1.raw &


----------------------
Troubleshooting
----------------------

(1) Updating repositories.

You may need to manually edit source/config.xml to remove repositories that are
not available or failing in your environment.

For example, to remove the 'openSUSE 12.1 Updates' repositories, remove the
following lines from config.xml:

  <repository type='rpm-md'>
    <source path='http://download.opensuse.org/update/12.1/'/>
  </repository>


(2) Build of SLE10 images fails with "image zypper version is too old".

The zypper version in SLE10 is missing features that Kiwi requires. As a
workaround, you can tell Kiwi to use only the host zypper version, which is
done by placing all packages into the bootstrap section and omitting the
<packages type="image"/> section.

Please refer to the suse-SLE10-JeOS template
(/usr/share/kiwi/image/suse-SLE10-JeOS/config.xml) in the kiwi-templates
package for details.


(3) Additional help.

For Studio and kiwi export specific questions, you can ask in the SUSE Studio
forum, which is linked to the studio-users mailing list
(http://susestudio.com/forum).

For kiwi specific questions, please ask in the kiwi-users mailing list
(http://lists.berlios.de/mailman/listinfo/kiwi-users).
