#!/bin/sh
# This is a copy of /usr/sbin/setup-chef-server.sh from redhat 5.6 
# after chef-server 10.2 has been installed.
# Currently un-modified (but renamed and copied locally).

echo "Checking RabbitMQ..."
rabbitmqctl list_vhosts > /dev/null 2>&1
if [ $? != 0 ]; then 
	echo "RabbitMQ not running. Starting..."
	chkconfig rabbitmq-server on
	service rabbitmq-server start
	if [ $? != 0 ]; then
		echo "Cannot start rabbitmq-server. Aborting."
		exit 1
	fi
fi

if [ -z "`rabbitmqctl list_vhosts | grep ^/chef`" ]; then
	echo "Configuring RabbitMQ default Chef user..."
	rabbitmqctl add_vhost /chef > /dev/null 2>&1
	rabbitmqctl add_user chef testing > /dev/null 2>&1
	rabbitmqctl set_permissions -p /chef chef ".*" ".*" ".*" > /dev/null 2>&1
fi

echo "Starting CouchDB..."
chkconfig couchdb on
service couchdb start
echo "Enabling Chef Services..."
for svc in server server-webui solr expander
do
  chkconfig chef-${svc} on 
done

# Some services do not start without the log dir created
mkdir /var/log/chef >/dev/null 2>&1
touch /var/log/chef/solr.log >/dev/null 2>&1
touch /var/log/chef/server.log >/dev/null 2>&1
touch /var/log/chef/server-webui.log >/dev/null 2>&1

echo "Starting Chef Services..."
for svc in server server-webui solr expander
do
  service chef-${svc} start
done

