
clean:
	@echo "Cleaning barclamp-provisioner"

distclean:
	@echo "Dist-Cleaning barclamp-provisioner"

all: clean build install

build:
	@echo "Building barclamp-provisioner"

install:
	@echo "Installing barclamp-provisioner"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-provisioner
	cp -r chef ${DESTDIR}/usr/share/barclamp-provisioner
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

