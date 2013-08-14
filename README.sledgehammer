Sledgehammer is the component of Crowbar that we use as a bootstrapping 
tool to perform initial node discovery and to register its discovered
state within the Crowbar node provisioning framework. After it has been
discovered Sledehammer can be controlled to prepare the hardward and to 
lay down the operating system and to assure correct node configuration.
It consists of a slightly modified Centos 6.2 live environment.  To build
Sledgehammer you need:

  * A CentOS 6.2 install DVD from bittorrent or your favorite CentOS
    mirror. This install DVD will need to go in $ISO_LIBRARY
    (usually $HOME/.crowbar-build-cache/iso).
  * Ruby, rpm, and rpm2cpio.

By default, the Crowbar build process will try and build Sledgehammer
if it is not already present in the build cache, so you should not
need to do anything special to build Sledgehammer.  If you need to
update Sledgehammer, run the build_sledgehammer.sh script in the main
Crowbar checkout.

Execute;
	#> sudo build_sledgehammer.sh
Output:
The build process creates an ISO image that will be emmbedded within an
initrd file.  The initrd file and all controls needed to use it will be
installed into the $ISO_LIBRARY/tftpboot directory (see ~/.build-crowbar.conf.

Note:
To debug build_sledgehammer.sh operation set in ~/.build-crowbar.conf
or in the environment:
	DEBUG=1
	SLEDGEECHO="echo"
This provides details logs and preserves intermediate files that are 
otherwise removed during processing. These files can be helpful for
diagnositc purposes BUT SLEDGEECHO should be null (empty) for prodction
use.
