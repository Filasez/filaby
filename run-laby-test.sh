#!/bin/bash
cd /home/filasez/Programme/filaby
export LABY_DATA=/home/filasez/Programme/filaby/data
echo "LABY_DATA=$LABY_DATA" > /tmp/laby-launch-test.log
echo "PWD=$(pwd)" >> /tmp/laby-launch-test.log
./_build-linux-amd64/src/laby.native >> /tmp/laby-launch-test.log 2>&1
