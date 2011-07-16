
clean:
	@echo "Cleaning barclamp-test"

distclean:
	@echo "Dist-Cleaning barclamp-test"

all: clean build install

build:
	@echo "Building barclamp-test"

install:
	@echo "Installing barclamp-test"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-test
	cp -r chef ${DESTDIR}/usr/share/barclamp-test
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

