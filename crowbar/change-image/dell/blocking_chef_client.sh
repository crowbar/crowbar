#!/bin/bash

lockfile="/tmp/chef-client.lock"
while ! ( set -o noclobber; echo "$$" > "$lockfile") 2> /dev/null; do
    pid=$(cat "$lockfile")
    echo "Failed to aquire lockfile: $lockfile."
    echo "Held by $(readlink "/proc/$pid/exe") ($pid)"
    echo "Process tree for $pid:" 
    pstree -Aupcl "$pid"
    echo
    sleep 1
done
trap 'rm -f "$lockfile"; exit $?' INT TERM EXIT
echo "$$" >"$lockfile"
ret=0
for loglvl in info debug; do
    chef-client -l "$loglvl" -V && break
    case $loglvl in
	info) echo "Chef client run failed, will retry with debugging.";;
	debug) echo "Chef client run failed with debug enabled."; ret=1;;
    esac
done
rm -f "$lockfile"
trap - INT TERM EXIT
exit $ret
