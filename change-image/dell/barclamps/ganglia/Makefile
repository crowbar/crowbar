
clean:
	@echo "Cleaning barclamp-ganglia"

distclean:
	@echo "Dist-Cleaning barclamp-ganglia"

all: clean build install

build:
	@echo "Building barclamp-ganglia"

install:
	@echo "Installing barclamp-ganglia"
	mkdir -p ${DESTDIR}/opt/crowbar/openstack_manager
	cp -r app ${DESTDIR}/opt/crowbar/openstack_manager
	mkdir -p ${DESTDIR}/usr/share/barclamp-ganglia
	cp -r chef ${DESTDIR}/usr/share/barclamp-ganglia
	mkdir -p ${DESTDIR}/usr/bin
	cp -r command_line/* ${DESTDIR}/usr/bin

