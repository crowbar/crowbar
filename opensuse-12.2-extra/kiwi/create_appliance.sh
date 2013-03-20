#!/bin/bash
# ============================================================================
# Script for creating appliances exported from SUSE Studio
# (http://susestudio.com) on your local system.
#
# Requires kiwi (http://opensuse.github.com/kiwi/).
#
# Author:  James Tan <jatan@suse.de>
# Contact: feedback@susestudio.com
# ============================================================================

image_file='image/Crowbar_2.0.x86_64-0.0.4'
image_arch='x86_64'
schema_ver='5.2'
base_system='12.2'
declare -a repos=()

dir="$(dirname $0)"
src="$dir/source"
dst="$dir/image"

if ! [ -d "$src/" ] || ! [ -f "$src/config.xml" ]; then
  printf "%s: %s\n" \
    "$0" "Cannot find appliance source." \
    "$0" "cd into the appliance directory and run './create_appliance.sh'." \
    >&2
  exit 1
fi

# Prints and runs the given command. Aborts if the command fails.
function run_cmd {
  command=$1
  echo $command
  $command
  if [ $? -ne 0 ]; then
    echo
    echo "** Appliance creation failed!"
    exit 1
  fi
}

# Display usage.
function usage {
  echo >&2 "Usage:"
  echo >&2 "  create_appliance.sh"
}

function url_unknown {
  local repo="${1?}"
  [ -f /etc/kiwi/repoalias ] || return 0
  ! grep -q "^{$repo}" /etc/kiwi/repoalias
}

function add_repo_url {
  local repo="${1?}"
  read -p "Enter repository URL for '$repo': " url
  mkdir -p /etc/kiwi
  echo "{$repo} $url" >> /etc/kiwi/repoalias \
  && echo "{$repo} $url alias added to /etc/kiwi/repoalias"
}

# Check that we're root.
if [ `whoami` != 'root' ]; then
  echo "Please run this script as root."
  exit 1
fi

# Check that kiwi is installed.
kiwi=`which kiwi 2> /dev/null`
if [ $? -ne 0 ]; then
  echo "Kiwi is required but not found on your system."
  echo "Run the following command to install kiwi:"
  echo
  echo "  zypper install kiwi kiwi-tools kiwi-desc-* kiwi-doc"
  echo
  exit 1
fi

echo "Note:  For a local build you will need a Kiwi version that supports building schemaversion $schema_ver or higher."

if [ "$base_system" = "12.1" ] || [ "$base_system" = "12.2" ]; then
  # Check system and it's version for 12.1 (it can't be built on SLES/SLED or
  # earlier versions of openSUSE).
  sys_name=`head -1 /etc/SuSE-release`
  sys_ver=`grep VERSION /etc/SuSE-release | sed -e 's/^[^=]*= *//'`
  if [ `echo "$sys_name" | grep -c openSUSE` -eq 0 -o `echo "$sys_ver < 12.1" | bc` -eq 1 ]; then
    echo "This appliance should be built on openSUSE 12.1 or newer (you use '$sys_name')."
    while true; do
      read -p "Continue? [y/n] " yn
      case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
      esac
    done
  fi
fi

# Check architecture (i686, x86_64).
sys_arch=`uname -m`
linux32=`which linux32 2>/dev/null`
if [ "$image_arch" = 'i686' ] && [ "$sys_arch" = 'x86_64' ]; then
  if [ "$linux32" = '' ]; then
    echo "'linux32' is required but not found."
    exit 1
  else
    kiwi="$linux32 $kiwi"
  fi
elif [ "$image_arch" = 'x86_64' ] && [ "$sys_arch" = 'i686' ]; then
  echo "Cannot build $image_arch image on a $sys_arch machine."
  exit 1
fi

# Replace internal repositories in config.xml.
echo "** Checking for internal repositories..."
for repo in "${repos[@]}"; do
  if grep -q "{$repo}" $src/config.xml && url_unknown "$repo"; then
    add_repo_url "$repo"
  fi
done

# Create appliance.
echo
echo "** Creating appliance..."
rm -rf build/root

run_cmd "$kiwi --build $src/ -d $dst"

# And we're done!
qemu_options='-snapshot'
[[ "$image_file" =~ \.iso$ ]] && qemu_options='-cdrom'
echo
echo "** Appliance created successfully! ($image_file)"
echo "To boot the image using qemu-kvm, run the following command:"
echo "  qemu-kvm -m 512 $qemu_options $image_file &"
echo
