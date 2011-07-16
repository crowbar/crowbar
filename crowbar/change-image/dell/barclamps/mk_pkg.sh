#!/bin/bash

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
