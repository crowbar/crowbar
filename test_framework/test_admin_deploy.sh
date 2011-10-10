#!/bin/bash
ssh root@192.168.124.10 -- /bin/bash -c 'test -f /var/log/install.log && ! pidof script &>/dev/null && ! grep -q "Admin node deployed" /var/log/install.log' 
