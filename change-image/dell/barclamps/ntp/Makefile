
clean:
	@echo "Cleaning barclamp-ntp"

distclean:
	@echo "Dist-Cleaning barclamp-ntp"

all: clean build install

build:
	@echo "Building barclamp-ntp"

install:
	@echo "Installing barclamp-ntp"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-ntp
	cp -r chef ${DESTDIR}/usr/share/barclamp-ntp
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

