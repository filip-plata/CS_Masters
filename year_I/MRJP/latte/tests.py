#!/usr/bin/env python3

import os
import sys
from pathlib import Path
import subprocess
import tempfile

from distutils.dir_util import copy_tree


BASE_DIR = os.path.dirname(os.path.realpath(__file__))
LLI_BINARY = "lli"
LLVM_COMPILER_NAME = "latc_llvm"
FRONTEND = "latc_frontend"
TEST_PROGRAMS_DIR = os.path.join(BASE_DIR, "testPrograms")
STACK_BASED = True


def build():
    subprocess.check_call(["stack", "build"])


def find_binary(name):
    out = subprocess.check_output(["stack", "exec", "--", "whereis", name]).decode("utf-8")
    return out.split()[1]


def llvm(filepath, inputs=None):
    subprocess.run([find_binary(LLVM_COMPILER_NAME), filepath])
    bytecode_filepath = os.path.splitext(filepath)[0] + ".bc"

    if inputs is not None:
        inputs = open(inputs, "r")

    completed = subprocess.run(
        [LLI_BINARY, bytecode_filepath],
        stdin=inputs,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        encoding='utf-8')
    if inputs:
        inputs.close()
    return completed.returncode, completed.stderr, completed.stdout


def frontend(filepath, **kwargs):
    completed = subprocess.run(
        [find_binary(FRONTEND), filepath],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        encoding='utf-8')
    return completed.returncode, completed.stderr, completed.stdout


def file_correct_out(filepath):
    out_filepath = os.path.splitext(filepath)[0] + ".output"
    try:
        with open(out_filepath, 'r') as content_file:
            return content_file.read().splitlines()
    except FileNotFoundError:
        return


def interpreter(filepath):
    return subprocess.check_output([find_binary(INTERPRETER_NAME), filepath]).decode("utf-8").splitlines()


def provide_correct_out(filepath):
    for output_provider in (file_correct_out, interpreter):
        res = output_provider(filepath)
        if res is not None:
            return res
    else:
        raise ValueError("Cannot provide output for file: " + filepath)


def test_bad_program(filepath):
    print(f"{filepath}\n")
    rc, stderr, stdout = frontend(filepath)
    assert rc != 0, f"Expecting non-zero return code for file {filepath}"
    assert "ERROR" in stderr, "Expecting ERROR in stderr"

    reasons_file = os.path.splitext(filepath)[0] + ".reason"

    with open(reasons_file, 'r') as r_f:
        for line in r_f:
            line = line.strip()
            assert (line in stderr or line in stdout), f"Expecting reason {line} to appear in output"


def test_good_program(filepath, runner=None, frontend_only=False):
    print(f"{filepath}\n")
    inputs_file = os.path.splitext(filepath)[0] + ".input"
    rc, stderr, stdout = runner(filepath, inputs=inputs_file if os.path.exists(inputs_file) else None)

    assert rc == 0, f"Expecting zero return code for file {filepath}"
    if frontend_only:
        assert "OK" in stderr, "Expecting OK in stderr"
        return

    outputs_file = os.path.splitext(filepath)[0] + ".output"
    with open(outputs_file, 'r') as o_f:
        expected = o_f.read()
        assert expected == stdout, "Output not as expected"



def test_frontend():
    for file_ in Path(os.path.join('testAdditional', 'bad', 'infinite_loop')).glob("*.lat"):
        test_bad_program(file_)
    for file_ in Path(os.path.join('testAdditional', 'bad', 'semantic')).glob("*.lat"):
        test_bad_program(file_)
    for file_ in Path(os.path.join('testPrograms', 'bad')).glob("*.lat"):
        test_bad_program(file_)


def test_good():
    for file_ in Path(os.path.join('testAdditional', 'good', 'basic')).glob("*.lat"):
        test_good_program(file_, runner=llvm)
    for file_ in Path(os.path.join('testPrograms', 'good')).glob("*.lat"):
        test_good_program(file_, runner=llvm)


def test_array():
    for file_ in Path(os.path.join('testAdditional', 'good', 'arrays')).glob("*.lat"):
        test_good_program(file_, runner=llvm)
    for file_ in Path(os.path.join('testPrograms', 'extensions', 'arrays1')).glob("*.lat"):
        test_good_program(file_, runner=llvm)


def test_struct():
    for file_ in Path(os.path.join('testPrograms', 'extensions', 'struct')).glob("*.lat"):
        test_good_program(file_, runner=llvm)


def test_object():
    for file_ in Path(os.path.join('testAdditional', 'good', 'virtual')).glob("*.lat"):
        test_good_program(file_, runner=llvm)
    for file_ in Path(os.path.join('testPrograms', 'extensions', 'objects1')).glob("*.lat"):
        test_good_program(file_, runner=llvm)
    for file_ in Path(os.path.join('testPrograms', 'extensions', 'objects2')).glob("*.lat"):
        test_good_program(file_, runner=llvm)


def main(argv):
    os.chdir(BASE_DIR)
    build()

    if "--frontend" in argv:
        test_frontend()

    if "--llvm" in argv:
        if "--good" in argv:
            test_good()
        if "--array" in argv:
            test_array()
        if "--struct" in argv:
            test_struct()
        if "--object" in argv:
            test_object()


if __name__ == "__main__":
    main(sys.argv)
