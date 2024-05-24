#!/bin/bash

set -e

rm -f /tmp/nc_fifo
mkfifo /tmp/nc_fifo

nc 127.0.0.1 7001 < /tmp/nc_fifo &
pid=$!

exec 3>/tmp/nc_fifo

input=$(echo -e "&9|marc\nsoda")
length=${#input}
i=0

while [ $i -lt $length ]; do
    chunk=${input:i:2}
    echo -e "$chunk"
    echo -n "$chunk" >&3
    i=$((i+2))
    sleep 1
done

kill $pid

exec 3>&-
rm /tmp/nc_fifo
