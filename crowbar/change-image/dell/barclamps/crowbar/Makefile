
clean:
	@echo "Cleaning barclamp-crowbar"

distclean:
	@echo "Dist-Cleaning barclamp-crowbar"

all: clean build install

build:
	@echo "Building barclamp-crowbar"

install:
	@echo "Installing barclamp-crowbar"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-crowbar
	cp -r chef ${DESTDIR}/usr/share/barclamp-crowbar
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

