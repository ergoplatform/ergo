#!/bin/bash

DEFAULT_NET_IP=`ifconfig eth0 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`

echo Default: $DEFAULT_NET_IP
echo Options: $OPTS

java $OPTS -jar /opt/ergo/ergo.jar /opt/ergo/template.conf
