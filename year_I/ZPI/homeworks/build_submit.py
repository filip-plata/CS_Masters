#!/usr/bin/env python3

import os
import sys
import shutil

print("Usage: %s PRESENTATION_PDF HOMEWORK_NUMBER" % (sys.argv[0]), file=sys.stderr)
assert len(sys.argv) == 3

BASE_DIR = os.path.dirname(os.path.realpath(__file__))
filepath = sys.argv[1]
homework_number = int(sys.argv[2])

def homework_name(number):
    return str(number).zfill(2) + "-PlataFilip.pdf"

_, file_extension = os.path.splitext(filepath)
assert file_extension == ".pdf"

shutil.move(filepath, os.path.join(BASE_DIR, "homework_submit", homework_name(homework_number)))
