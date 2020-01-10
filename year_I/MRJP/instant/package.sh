#!/usr/bin/env bash

set -o nounset

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
TARGET_DIR=$1

cp -r ${DIR}/src/ ${TARGET_DIR}/src/
cp -r ${DIR}/app/ ${TARGET_DIR}/app/

cp ${DIR}/Makefile ${TARGET_DIR}
cp ${DIR}/README.md ${TARGET_DIR}
cp ${DIR}/LICENSE ${TARGET_DIR}
cp ${DIR}/tests.py ${TARGET_DIR}
cp ${DIR}/instant.cabal ${TARGET_DIR}
cp -r ${DIR}/lib ${TARGET_DIR}/
cp -r ${DIR}/testPrograms ${TARGET_DIR}/

tar -czvf fp371335.tar.gz ${TARGET_DIR}
