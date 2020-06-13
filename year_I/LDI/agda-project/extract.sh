#!/bin/bash

id=$1
input=$2

echo $id
echo $input

awk "BEGIN { skip = 0;}\
/^module submission where/ { print module test$id where; next;}\
/^-- BEGIN PROBLEM p$id/ { skip = 0; next;}\
/^-- BEGIN PROBLEM/ {skip = 1;next;}\
/^-- END PROBLEM/ {skip = 0;next;}\
{ if(skip!=1)print;}" $input