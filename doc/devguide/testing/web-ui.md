### Manually testing the web UI

Firstly [set up your development environment](devtool.md).

#### Starting the Crowbar web interface

   ````
   cd /tmp/crowbar-dev-test/opt/dell/crowbar_framework
   bundle exec rails s puma
   ````

You will want to keep this terminal open to see the Rails logs, which will
come in very handy during development and debugging. The server can be
terminated with `Ctrl-C`.

The Crowbar web interface should now be accessible from your host web
browser, e.g. at: http://192.168.124.10:3000

#### Dashboard No Polling

When you are troubleshooting the UI or REST APIs, the Node Dashboard (`dashboard`) polling can be a pain because it generates log traffic.  You can disable polling for debug by using the `nopoll` parameter.

For example: http://192.168.124.10:3000/dashboard/89?nopoll
