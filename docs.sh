#!/bin/bash

# Start rails
cd /tmp/crowbar-dev-test/opt/dell/crowbar_framework
bundle exec script/rails s Puma >> log/test.out 2>> log/test.err &
sleep 10
cd ../doc

# Run Book Gen
for book in [gettingstarted releasenotes userguide barclamps deployguide devguide licenses]; do
  curl "http://127.0.0.1:3000/docs/topic/framework/$book?expand=true" -o "$book.html"
done

# Stop rails
kill `ps -ef | grep Puma | grep ruby | awk '{ print $2 }'`

