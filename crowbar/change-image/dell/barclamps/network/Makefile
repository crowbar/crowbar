
clean:
	@echo "Cleaning barclamp-network"

distclean:
	@echo "Dist-Cleaning barclamp-network"

all: clean build install

build:
	@echo "Building barclamp-network"

install:
	@echo "Installing barclamp-network"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-network
	cp -r chef ${DESTDIR}/usr/share/barclamp-network
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

