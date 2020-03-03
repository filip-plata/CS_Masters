#!/usr/bin/env bash

BUILD_DIR=bin

rm -rf ${BUILD_DIR}
mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}

cmake ..
make
