#!/usr/bin/env python3

import os
from pathlib import Path
import subprocess
import tempfile

from distutils.dir_util import copy_tree


BASE_DIR = os.path.dirname(os.path.realpath(__file__))
JAVA_BINARY = "java"
LLI_BINARY = "lli"
INTERPRETER_NAME = "insc_interpreter"
JVM_COMPILER_NAME = "insc_jvm"
LLVM_COMPILER_NAME = "insc_llvm"
TEST_PROGRAMS_DIR = os.path.join(BASE_DIR, "testPrograms")
STACK_BASED = True


def build():
    subprocess.check_call(["stack", "build"])


def find_binary(name):
    out = subprocess.check_output(["stack", "exec", "--", "whereis", name]).decode("utf-8")
    return out.split()[1]


def jvm(filepath):
    subprocess.run([find_binary(JVM_COMPILER_NAME), filepath])
    dir, file_ = os.path.split(filepath)
    class_filepath = os.path.splitext(file_)[0]
    return subprocess.check_output([JAVA_BINARY, "-classpath", dir, class_filepath]).decode("utf-8").splitlines()


def llvm(filepath):
    subprocess.run([find_binary(LLVM_COMPILER_NAME), filepath])
    bytecode_filepath = os.path.splitext(filepath)[0] + ".bc"
    return subprocess.check_output([LLI_BINARY, bytecode_filepath]).decode("utf-8").splitlines()


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


def test_program(filepath, compilers=(jvm, llvm)):
    print(filepath)
    correct_out = provide_correct_out(filepath)

    for compiler in compilers:
        compiler_out = compiler(filepath)
        for (idx, (correct, test)) in enumerate(zip(correct_out, compiler_out)):
            assert correct == test, f"Input file {filepath}, line {idx + 1}, expected: {correct}, received: {test}"


def main():
    with tempfile.TemporaryDirectory(prefix="MRJP") as dir:
        copy_tree(BASE_DIR, dir)
        os.chdir(dir)
        build()

        for file_ in Path('testPrograms').glob("*.ins"):
            test_program(file_)



if __name__ == "__main__":
    main()
