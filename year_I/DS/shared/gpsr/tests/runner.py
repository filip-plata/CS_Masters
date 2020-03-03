#!/usr/bin/env python3

import os
import re
import sys
import shutil, glob
import subprocess

from pathlib import Path


BASE_DIR = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR = str(Path(BASE_DIR).parent)
WORK_DIR = os.path.join(BASE_DIR, "build")


class TestDescription:
    def __init__(self, file_):
        with open(file_, 'r') as f:
            self.motes = int(f.readline())
            self.motes_info = []
            for i in range(self.motes):
                rec_id, rec_long, rec_lat = map(int, f.readline().split())
                self.motes_info.append((rec_id, rec_long, rec_lat))


def generate_simple_sensor(target_dir, test_desc):
    shutil.copy2(os.path.join(BASE_DIR, "SimpleSophisticatedSensorP.nc.template"),
                 os.path.join(target_dir, "SimpleSophisticatedSensorP.nc"))

    location_switch = ["switch (TOS_NODE_ID) {"]

    for i in range(test_desc.motes):
        recipient = test_desc.motes_info[i][0]
        location_switch.append("case %d: recipientId = %d; recipient_long = %d; recipient_lat = %d; break;" % (i, recipient, test_desc.motes_info[recipient][1], test_desc.motes_info[recipient][2]))

    location_switch.append("}")
    location_switch = "\\n".join(location_switch)

    subprocess.check_call(
        ["sed", "-i", "s/RECIPIENT_SWITCH_DEVICE/%s/g" % location_switch,
        os.path.join(target_dir, "SimpleSophisticatedSensorP.nc")])


def generate_simple_location(target_dir, test_desc):
    shutil.copy2(os.path.join(BASE_DIR, "SimpleLocationP.nc.template"),
                 os.path.join(target_dir, "SimpleLocationP.nc"))

    location_switch = ["switch (TOS_NODE_ID) {"]

    for i in range(test_desc.motes):
        location_switch.append("case %d: self_long = %d; self_lat = %d; break;" % (i, test_desc.motes_info[i][1], test_desc.motes_info[i][2]))

    location_switch.append("}")
    location_switch = "\\n".join(location_switch)

    subprocess.check_call(
        ["sed", "-i", "s/LOCATION_SWITCH_DEVICE/%s/g" % location_switch,
        os.path.join(target_dir, "SimpleLocationP.nc")]
    )


def setup_tossim(target_dir, test_description_file):
    test_name = os.path.basename(test_description_file)
    files = list(glob.iglob(os.path.join(ROOT_DIR, "*.nc")))
    files += [os.path.join(ROOT_DIR, "Radio.h"),
              os.path.join(ROOT_DIR, "Makefile"),
              os.path.join(BASE_DIR, "meyer-heavy.txt"),
              os.path.join(BASE_DIR, "sim.py"),
              os.path.join(BASE_DIR, "SimpleReadingsStoreP.nc"),
              os.path.splitext(test_description_file)[0] + ".gains"]
    for file in files:
        if os.path.isfile(file):
            shutil.copy2(file, target_dir)

    test_description = TestDescription(test_description_file)

    generate_simple_location(target_dir, test_description)
    generate_simple_sensor(target_dir, test_description)


if __name__ == "__main__":
    shutil.rmtree(WORK_DIR, ignore_errors=True)
    os.mkdir(WORK_DIR)

    for test_name in glob.iglob(os.path.join(BASE_DIR, "*.test")):
        test_name = os.path.basename(test_name)
        os.chdir(WORK_DIR)
        os.mkdir(test_name)
        current_test_dir = os.path.join(WORK_DIR, test_name)
        setup_tossim(current_test_dir, os.path.join(BASE_DIR, test_name))
        os.chdir(current_test_dir)
