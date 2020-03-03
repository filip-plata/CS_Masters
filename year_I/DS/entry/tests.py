#!/usr/bin/env python3

import os
import subprocess
import numexpr
import random
import argparse
import signal, psutil

BASE_DIR = os.path.dirname(os.path.realpath(__file__))
BUILD_SCRIPT = os.path.join(BASE_DIR, "build.sh")
SERVER_SCRIPT = os.path.join(BASE_DIR, "server.sh")
CLIENT_SCRIPT = os.path.join(BASE_DIR, "client.sh")

DEFAULT_PORT = 6868
DEFAULT_THREADS = 3
DEFAULT_SERVER_HOST = "127.0.0.1"

ERROR_RESPONSE = "ERROR\n"
TIMEOUT_RESPONSE = "TIMEOUT\n"


def kill_child_processes(parent_pid, sig=signal.SIGTERM):
    try:
        parent = psutil.Process(parent_pid)
    except psutil.NoSuchProcess:
        return
    children = parent.children(recursive=True)
    for process in children:
        process.send_signal(sig)


def build_solution():
    subprocess.check_call([BUILD_SCRIPT])


def start_server(port=DEFAULT_PORT, threads=DEFAULT_THREADS):
    return subprocess.Popen([SERVER_SCRIPT, "-p", str(port), "-t", str(threads)])


def client_send_string(data, port=DEFAULT_PORT, host=DEFAULT_SERVER_HOST):
    client = subprocess.Popen([CLIENT_SCRIPT, "-p", str(port), "-a", host], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    client.stdin.write(data.encode('utf-8'))
    client.stdin.close()
    return client.stdout.read()


def generate_random_expression(length=100, max_number=1e12, positive_only=False):
    tokens = [str(random.randint(0, max_number))]

    for _ in range(length):
        if positive_only:
            tokens.append("+")
        else:
            tokens.append("+" if bool(random.getrandbits(1)) else "-")
        tokens.append(''.join(random.choice(" \t") for _ in range(20)))
        tokens.append(str(random.randint(0, max_number)))

    return ''.join(tokens)


def test_smoke():
    valid_exprs = ['1 + 1', '1- 3\t+10-111111+13', '11 \t + 8', "0 + 0"]
    invalid_exprs = ['1+-3', "+1", "-1", "1+3-", "2 * 2", "2 2", "2 + 010", "00 + 0"]

    for expr in valid_exprs:
        assert int(client_send_string(expr)) == numexpr.evaluate(expr).item()

    for expr in invalid_exprs:
        assert client_send_string(expr).decode("utf-8") == ERROR_RESPONSE


def test_random_expressions(expressions=10):
    for _ in range(expressions):
        expr = generate_random_expression()
        expected = numexpr.evaluate(expr).item()
        obtained = int(client_send_string(expr))
        assert obtained == expected, "Obtained: %s Expected: %s" % (str(obtained), str(expected))


def test_long_expression(length=100):
    client = subprocess.Popen([CLIENT_SCRIPT, "-p", str(DEFAULT_PORT), "-a", DEFAULT_SERVER_HOST], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    result = 0

    for i in range(length):
        expr = generate_random_expression(length=1000, max_number=10e6, positive_only=False)
        client.stdin.write(expr.encode('utf-8'))
        if i < length - 1:
            client.stdin.write("+".encode('utf-8'))
        result += numexpr.evaluate(expr).item()

    client.stdin.close()
    assert result == int(client.stdout.read())


def test_big_int(max_number=1e16, length=100):
    client = subprocess.Popen([CLIENT_SCRIPT, "-p", str(DEFAULT_PORT), "-a", DEFAULT_SERVER_HOST], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    result = 0

    for i in range(length):
        expr = generate_random_expression(length=10, max_number=max_number, positive_only=(random.uniform(0, 1) > 0.25))
        client.stdin.write(expr.encode('utf-8'))
        if i < length - 1:
            client.stdin.write("+".encode('utf-8'))
        result += numexpr.evaluate(expr).item()

    client.stdin.close()
    assert result == int(client.stdout.read())


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--host")
    return parser.parse_args()


def main():
    build_solution()
    server = start_server() if DEFAULT_SERVER_HOST == "127.0.0.1" else None
    test_smoke()
    test_random_expressions()
    test_long_expression()
    test_big_int()
    print("Success")
    if server:
        kill_child_processes(server.pid)
        server.kill()


if __name__ == "__main__":
    args = parse_args()
    if args.host:
        DEFAULT_SERVER_HOST = args.host
    main()
