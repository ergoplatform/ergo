#!/bin/bash

DEFAULT_NET_IP=`ifconfig eth0 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`
INNER_NET_IP=`ifconfig eth1 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`

echo Default: $DEFAULT_NET_IP
echo Inner: $INNER_NET_IP
echo Options: $OPTS

java -Dwaves.network.declared-address=$INNER_NET_IP:$PORT $OPTS -jar /opt/ergo/ergo.jar /opt/ergo/template.conf
