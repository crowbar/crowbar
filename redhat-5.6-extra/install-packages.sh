#!/bin/bash
#
# Script: install-packages.sh
#
# Copyright (c) 2011 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Install the rpm and gem packages

# Internet access flag (download packages from the public repos).
inet_access=true

# If true, stdout & stderr goes to the console, otherwise, use log files
use_console=true

# Run a command and log its output.
log_to() {
    # $1 = install log to log to
    # $@ = rest of args
    local __logname="$1" _ret=0
    local __log="/var/log/install-$1"
    local __timestamp="$(date '+%F %T %z')"
    local log_skip_re='^gem|knife$'
    shift
    if [[ $use_console = true ]]; then
      "$@" || {
  	_ret=$?
      }
    else 
      printf "\n%s\n" "$__timestamp: Running $*" | \
     	  tee -a "$__log.err" >> "$__log.log"
          "$@" 2>> "$__log.err" >>"$__log.log" || {
 	  _ret=$?
	  if ! [[ $__logname =~ $log_skip_re ]]; then
	      echo "$__timestamp: $* failed."
	      echo "See $__log.log and $__log.err for more information."
	  fi
      }
      printf "\n$s\n--------\n"  "$(date '+%F %T %z'): Done $*" | \
  	  tee -a "$__log.err" >> "$__log.log"
    fi
    return $_ret
}

########## FIXME
# Haven't looked into yet - maybe there's an equivilent.
# log_to yum sed -i "s/__HOSTNAME__/$FQDN/g" ./debsel.conf
# log_to yum /usr/bin/debconf-set-selections ./debsel.conf
#########################################################

# log_to yum yum -y install chef chef-server chef-server-webui \
#    libnet-ssh-multi-ruby kwalify libcurl3-dev build-essential ruby-dev
# log_to yum yum -y localinstall rpms/rubygem-kwalify-0.7.2-3.el5.noarch.rpm

# Download rpms if connected to the Internet
if [[ $inet_access = true ]]; then
  # Chef server package (10.2 - RBEL distrubution)
  # You need to have the standard red hat OS rpm's mounted
  # and configured (/etc/yum.repo.d) to resolve any rpm dependancies.
  # I have the redhat OS cd iso mounted in the cd drive
  # to do this.
  rpm -Uvh http://rbel.frameos.org/rbel5
  yum install rubygem-chef-server
else
[[ ! -f /etc/pki/rpm-gpg/RPM-GPG-KEY-RBEL ]]
  cp ./templates/RPM-GPG-KEY-RBEL /etc/pki/rpm-gpg
fi

# Download gems if internet access enabled.
# We install from a local repo in the next code block.
# Note : Some of these packages were installed as gems before (ubuntu-10.10-extra dist)
# and now they are part of the chef 10.2 rpm installation.
# Check the rpm packages first before adding gems !!!
# The following packages are now installed from rpms:
#   rubygem-activesupport-3.0.3-1.el5.noarch.rpm
#   rubygem-haml-3.1.1-1.el5.noarch.rpm rubygem-merb-haml-1.1.3-1.el5.noarch.rpm
#   rubygem-json-1.4.6-1.el5.x86_64.rpm

if [[ $inet_access = true ]]; then
  echo "$(date '+%F %T %z'): Downloading Gems..."
# GEMS=(kwalify simple-navigation syslogger)
  GEMS=(i18n patron sass simple-navigation kwalify syslogger)
  for g in "${GEMS[@]}"; do
    cd gems
    echo "fetch $g"
    log_to gem gem fetch "$g"
    cd ..
  done
fi

# Install ruby gems
echo "$(date '+%F %T %z'): Installing Gems..."
for g in gems/*.gem; do
    echo "install $g"
    log_to gem gem install --local --no-ri --no-rdoc $g
done

exit 0

