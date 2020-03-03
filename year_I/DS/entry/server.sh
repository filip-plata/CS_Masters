#!/usr/bin/env bash

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    -t)
    THREADS="$2"
    shift # past argument
    shift # past value
    ;;
    -p)
    PORT="$2"
    shift # past argument
    shift # past value
    ;;
esac
done

./bin/server ${PORT} ${THREADS}
