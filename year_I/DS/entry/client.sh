#!/usr/bin/env bash

TIMEOUT=5

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    -a)
    ADDR="$2"
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

./bin/client ${ADDR} ${PORT} ${TIMEOUT}
