
clean:
	@echo "Cleaning barclamp-nagios"

distclean:
	@echo "Dist-Cleaning barclamp-nagios"

all: clean build install

build:
	@echo "Building barclamp-nagios"

install:
	@echo "Installing barclamp-nagios"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-nagios
	cp -r chef ${DESTDIR}/usr/share/barclamp-nagios
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

