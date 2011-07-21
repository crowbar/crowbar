
clean:
	@echo "Cleaning barclamp-nova"

distclean:
	@echo "Dist-Cleaning barclamp-nova"

all: clean build install

build:
	@echo "Building barclamp-nova"

install:
	@echo "Installing barclamp-nova"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-nova
	cp -r chef ${DESTDIR}/usr/share/barclamp-nova
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

