#!/bin/bash

cp tsung-1.0.dtd $1/
cp include/ts_qmsg.hrl $1/include/
cp src/tsung_controller/ts_config_qmsg.erl $1/src/tsung_controller/
cp src/tsung/ts_qmsg.erl $1/src/tsung/

cd $1/
./configure --prefix=/usr/local
make install
