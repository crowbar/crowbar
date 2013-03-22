#!/bin/bash

DIR=`dirname "${BASH_SOURCE[0]}"`
cd "$DIR/kiwi"
sudo ./create_appliance.sh
