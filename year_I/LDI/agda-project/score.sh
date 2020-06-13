#!/bin/bash

# echo running Agda on $1
agda -W error --safe --no-main test$1.lagda.md

if (( $? == 0 )) ; then
    echo 1 > result$1.txt
else
    echo 0 > result$1.txt
    exit 1
fi