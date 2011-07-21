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

