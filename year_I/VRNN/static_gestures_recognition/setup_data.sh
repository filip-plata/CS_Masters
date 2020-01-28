#!/usr/bin/env bash

ASL_DATA_DIR=$(pwd)/"data-static"
DATA_DIR=$(pwd)/"data-static"

mkdir -p "${DATA_DIR}/train"
mkdir -p "${DATA_DIR}/test"

cd "${ASL_DATA_DIR}"/asl_alphabet_train/asl_alphabet_train/ || exit

for class in *
do
  cp -r "${class}" "${DATA_DIR}/train/"
  mkdir -p "${DATA_DIR}/test/${class}"

  for test_idx in $(shuf -i 1-3000 -n 300)
  do
    mv "${DATA_DIR}/train/${class}/${class}${test_idx}.jpg" "${DATA_DIR}/test/${class}"
  done
done