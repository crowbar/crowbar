#!/bin/bash

# Run Book Gen
for book in gettingstarted releasenotes userguide barclamps deployguide devguide licenses; do
  curl "http://127.0.0.1:3000/docs/topic/framework/$book?expand=true" -o "/tmp/crowbar-dev-test/opt/dell/doc/$book.html" -#
  echo "exported /tmp/crowbar-dev-test/opt/dell/doc/$book.html"
done


