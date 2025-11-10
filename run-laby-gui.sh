#!/bin/bash
LOGFILE=/tmp/laby-launch-test.log
echo "Starting Laby..." > "$LOGFILE"
echo "PWD=$(pwd)" >> "$LOGFILE"
echo "LABY_DATA=/home/filasez/Programme/filaby/data" >> "$LOGFILE"
export LABY_DATA=/home/filasez/Programme/filaby/data
export DISPLAY=:0
/home/filasez/Programme/filaby/_build-linux-amd64/src/laby.native >> "$LOGFILE" 2>&1
echo "Finished" >> "$LOGFILE"

