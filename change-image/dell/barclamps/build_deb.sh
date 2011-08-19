# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# Must have tools:
# apt-get install dpkg-dev debhelper devscripts fakeroot linda dh-make
#

. ./version.sh

sed -e "s/CHANGE_LOG_LINE/barclamp-${BARCLAMP_NAME} (${MAJOR_VERSION}.${MINOR_VERSION}-${DEB_CONTEXT_NUMBER}) unstable; urgency=low/" debian/changelog.tmpl > debian/changelog


yes | debuild -us -uc

mkdir -p bin
mv ../barclamp-${BARCLAMP_NAME}_*.deb bin
mv ../barclamp-${BARCLAMP_NAME}_*gz bin
rm ../barclamp-${BARCLAMP_NAME}_*

