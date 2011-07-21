
%define _topdir         BUILD_DIR
%define name		barclamp-nagios
%define release		RPM_CONTEXT_NUMBER
%define version 	MAJOR_VERSION.MINOR_VERSION
%define buildroot	%{_topdir}/%{name}-%{version}-root

BuildRoot:		%{buildroot}
Summary: 		common Nagios service for the cluster that can be used by other barclamps
License: 		Apache 2.0
Name: 			%{name}
BuildArch:		noarch
Version: 		%{version}
Release: 		%{release}
Source: 		%{name}-%{version}.tar.gz
Prefix: 		/
Group: 			Development/Tools

%description
A Crowbar Barclamp that manages nagios deployments within a Crowbar environment.

%prep
%setup -q

%build

%install
make install DESTDIR=${RPM_BUILD_ROOT}

%post 
cd /usr/share/barclamp-nagios/chef/cookbooks
knife cookbook upload -o . -a -u chef-webui -k /etc/chef/webui.pem

cd /usr/share/barclamp-nagios/chef/data_bags/crowbar
for i in *.json; do
    knife data bag from file crowbar $i
done

cd /usr/share/barclamp-nagios/chef/roles
for i in *.rb; do
    knife role from file $i
done

service httpd graceful


%files
%defattr(-,root,root)
/usr/bin
/usr/share
/opt

