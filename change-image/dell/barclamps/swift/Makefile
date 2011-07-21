
clean:
	@echo "Cleaning barclamp-swift"

distclean:
	@echo "Dist-Cleaning barclamp-swift"

all: clean build install

build:
	@echo "Building barclamp-swift"

install:
	@echo "Installing barclamp-swift"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-swift
	cp -r chef ${DESTDIR}/usr/share/barclamp-swift
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

