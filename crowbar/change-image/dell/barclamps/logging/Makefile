
clean:
	@echo "Cleaning barclamp-logging"

distclean:
	@echo "Dist-Cleaning barclamp-logging"

all: clean build install

build:
	@echo "Building barclamp-logging"

install:
	@echo "Installing barclamp-logging"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-logging
	cp -r chef ${DESTDIR}/usr/share/barclamp-logging
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

