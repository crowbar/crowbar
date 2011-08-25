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

clean:
	@echo "Cleaning barclamp-nova"

distclean:
	@echo "Dist-Cleaning barclamp-nova"

all: clean build install

build:
	@echo "Building barclamp-nova"

install:
	@echo "Installing barclamp-nova"
	mkdir -p ${DESTDIR}/opt/crowbar/crowbar_framework
	cp -r app ${DESTDIR}/opt/crowbar/crowbar_framework
	mkdir -p ${DESTDIR}/usr/share/barclamp-nova
	cp -r chef ${DESTDIR}/usr/share/barclamp-nova
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

