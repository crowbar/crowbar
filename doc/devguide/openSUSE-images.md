# openSUSE images

There are a number of openSUSE images available for different use cases:

1. [Crowbar Dev VM](http://susestudio.com/a/n0rKOx/crowbar-dev). This is a
   ready to go Crowbar development image, based on openSUSE. Tailored for those
   who want to work on the Crowbar Rails application directly from Git sources.
   Follow the [step-by-step instructions]
   (https://github.com/crowbar/crowbar/blob/master/doc/devguide/dev-vm-openSUSE.md)
   and start hacking.

1. [Crowbar test image](http://susestudio.com/a/E5zfDp/crowbar-2-0). This
   image uses the packages from [systemsmanagement:crowbar:2.0]
   (https://build.opensuse.org/project/monitor?project=systemsmanagement:crowbar:2.0)
   and is intended only for developer testing.

1. [Crowbar staging test image]
   (http://download.opensuse.org/repositories/systemsmanagement:/crowbar:/2.0:/staging/images/).
   This image is similar to the one above, but uses the packages from
   [systemsmanagement:crowbar:2.0:staging]
   (https://build.opensuse.org/project/monitor?project=systemsmanagement%3Acrowbar%3A2.0%3Astaging).
   The image is automatically rebuilt by [OBS](https://build.opensuse.org/)
   whenever there are new packages/RPMs in the staging repository. Also
   intended only for developer testing.
