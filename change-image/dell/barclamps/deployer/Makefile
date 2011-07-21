
clean:
	@echo "Cleaning barclamp-deployer"

distclean:
	@echo "Dist-Cleaning barclamp-deployer"

all: clean build install

build:
	@echo "Building barclamp-deployer"

install:
	@echo "Installing barclamp-deployer"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-deployer
	cp -r chef ${DESTDIR}/usr/share/barclamp-deployer
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

