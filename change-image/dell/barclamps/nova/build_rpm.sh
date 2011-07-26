#!/bin/bash 
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
# Needs sudo apt-get install rpm
#

. ./version.sh

BUILD_DIR="/tmp/build.$$"
rm -rf ${BUILD_DIR}
mkdir -p ${BUILD_DIR}/BUILD
mkdir -p ${BUILD_DIR}/RPMS
mkdir -p ${BUILD_DIR}/SOURCES
mkdir -p ${BUILD_DIR}/SPECS
mkdir -p ${BUILD_DIR}/SRPMS

FULL_NAME="barclamp-${BARCLAMP_NAME}-${MAJOR_VERSION}.${MINOR_VERSION}"

mkdir $FULL_NAME
cp -r Makefile app chef command_line $FULL_NAME
tar -zcf ${BUILD_DIR}/SOURCES/${FULL_NAME}.tar.gz ${FULL_NAME}
rm -rf ${FULL_NAME}

sed -e "s%BUILD_DIR%$BUILD_DIR%" barclamp-${BARCLAMP_NAME}.spec > ${BUILD_DIR}/SPECS/barclamp-${BARCLAMP_NAME}.spec
sed -ie "s%MAJOR_VERSION%${MAJOR_VERSION}%" ${BUILD_DIR}/SPECS/barclamp-${BARCLAMP_NAME}.spec
sed -ie "s%MINOR_VERSION%${MINOR_VERSION}%" ${BUILD_DIR}/SPECS/barclamp-${BARCLAMP_NAME}.spec
sed -ie "s%RPM_CONTEXT_NUMBER%${RPM_CONTEXT_NUMBER}%" ${BUILD_DIR}/SPECS/barclamp-${BARCLAMP_NAME}.spec

rpmbuild -v -ba --clean ${BUILD_DIR}/SPECS/barclamp-${BARCLAMP_NAME}.spec

mkdir -p bin
cp ${BUILD_DIR}/RPMS/noarch/* bin
cp ${BUILD_DIR}/SRPMS/* bin
#rm -rf ${BUILD_DIR}
