
clean:
	@echo "Cleaning barclamp-glance"

distclean:
	@echo "Dist-Cleaning barclamp-glance"

all: clean build install

build:
	@echo "Building barclamp-glance"

install:
	@echo "Installing barclamp-glance"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-glance
	cp -r chef ${DESTDIR}/usr/share/barclamp-glance
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

