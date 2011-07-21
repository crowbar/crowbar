
clean:
	@echo "Cleaning barclamp-dns"

distclean:
	@echo "Dist-Cleaning barclamp-dns"

all: clean build install

build:
	@echo "Building barclamp-dns"

install:
	@echo "Installing barclamp-dns"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-dns
	cp -r chef ${DESTDIR}/usr/share/barclamp-dns
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

