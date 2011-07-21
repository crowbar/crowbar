#!/bin/bash

[[ -f /tmp/deploying ]] && exit 0

lockfile="/tmp/looper-chef-client.lock"

if ( set -o noclobber; echo "$$" > "$lockfile") 2> /dev/null; then
    trap 'rm -f "$lockfile"; exit $?' INT TERM EXIT
    
    while true; do
	rm -f /tmp/chef-client.run
	
	/opt/dell/bin/blocking_chef_client.sh
	
	while [[ ! -f /tmp/chef-client.run ]]
	do
	    sleep 1
	done
    done

  # Ideally these won't be hit.
    rm -f "$lockfile"
    trap - INT TERM EXIT
    exit 0
else
    touch /tmp/chef-client.run
fi

