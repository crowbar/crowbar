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


# copies the ganglia packing scripts into the specified directory making the changes for the name.

NEW_PKG=$1

cd ganglia
FILE_LIST=`find debian version.sh Makefile build_* barclamp* -type f | grep -v svn`
cd ..

cd $1
mkdir -p debian/source

svn delete debian/changelog
svn delete debian/files

for i in $FILE_LIST 
do
  file=`echo $i | sed -e "s/ganglia/$1/"`
  echo $file

  cp ../ganglia/$i $file
  
  sed -i -e "s/ganglia/$1/g" $file
done

svn add version.sh
svn add debian/changelog.tmpl

cd -
