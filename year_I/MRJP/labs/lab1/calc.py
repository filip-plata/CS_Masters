import sys
import re

MAX_STACK_SIZE = 1024

class BinaryOp:
    def __str__(self):
        return "(%s) %s (%s)" % (str(self.children[0]), self.label, str(self.children[1]))

class UnaryOp:
    def __str__(self):
        return "%s (%s)" % (self.label, str(self.children[0]))

class AddNode(BinaryOp):
    def __init__(self, left, right):
        self.children = [left, right]
        self.label = "+"

    def __str__(self):
        return "%s %s %s" % (str(self.children[0]), self.label, str(self.children[1]))

class MulNode(BinaryOp):
    def __init__(self, left, right):
        self.children = [left, right]
        self.label = "*"

class DivideNode(BinaryOp):
    def __init__(self, left, right):
        self.children = [left, right]
        self.label = "/"

class MinusNode(BinaryOp):
    def __init__(self, left, right):
        self.children = [left, right]
        self.label = "-"

class ModulloNode(BinaryOp):
    def __init__(self, left, right):
        self.children = [left, right]
        self.label = "%"

class NumNode:
    def __init__(self, num):
        self.children = []
        self.label = str(num)

    def __str__(self):
        return self.label

class WriteNode(UnaryOp):
    def __init__(self, num_node):
        self.children = [num_node]
        self.label = "WRITE"

def bin_op(stack, op):
    try:
        a = stack.pop()
        b = stack.pop()
    except:
        raise ValueError("Not enough operands, two required")
    stack.append(op(a, b))

def plus(a, b):
    return a + b

def minus(a, b):
    return a - b

def mul(a, b):
    return a * b

def divide(a, b):
    return a / b

def modullo(a, b):
    return a % b

def add_node(a, b):
    return AddNode(a, b)

def mul_node(a, b):
    return MulNode(a, b)

def div_node(a, b):
    return DivideNode(a, b)

def minus_node(a, b):
    return MinusNode(a, b)

def modullo_node(a, b):
    return ModulloNode(a, b)


def build_op_tree(stack, op, env):
    if len(stack) > env["MAX_STACK_SIZE"]:
        raise RuntimeError("Stack overflow")
    if op.isdigit():
        stack.append(NumNode(int(op)))
    elif op == "+":
        bin_op(stack, add_node)
    elif op == "WRITE":
        stack.append(WriteNode(stack.pop()))
    elif op == "*":
        bin_op(stack, mul_node)
    elif op == "-":
        bin_op(stack, minus_node)
    elif op == "/":
        bin_op(stack, div_node)
    elif op == "%":
        bin_op(stack, modullo_node)
    elif op == "STACK":
        env["MAX_STACK_SIZE"] = int(stack.pop())
    elif op == "LOAD":
        var = stack.pop()
        if var not in env:
            raise ValueError("Variable " + var + " not defined")
        stack.append(env[var])
    elif op == "STORE":
        num = stack.pop()
        var = stack.pop()
        if var.isalpha():
            try:
                env[var] = int(num)
            except:
                raise ValueError("Trying to store: " + num + " which is not a number")
    elif op.isalpha():
        stack.append(op)


def handle_op(stack, op, env):
    if len(stack) > env["MAX_STACK_SIZE"]:
        raise RuntimeError("Stack overflow")
    if op.isdigit():
        stack.append(int(op))
    elif op == "+":
        bin_op(stack, plus)
    elif op == "WRITE":
        print(int(stack.pop()))
    elif op == "*":
        bin_op(stack, mul)
    elif op == "-":
        bin_op(stack, minus)
    elif op == "/":
        bin_op(stack, divide)
    elif op == "%":
        bin_op(stack, modullo)
    elif op == "STACK":
        env["MAX_STACK_SIZE"] = int(stack.pop())
    elif op == "LOAD":
        var = stack.pop()
        if var not in env:
            raise ValueError("Variable " + var + " not defined")
        stack.append(env[var])
    elif op == "STORE":
        num = stack.pop()
        var = stack.pop()
        if var.isalpha():
            try:
                env[var] = int(num)
            except:
                raise ValueError("Trying to store: " + num + " which is not a number")
    elif op.isalpha():
        stack.append(op)

for line in sys.stdin:
    stack = []
    env = {"MAX_STACK_SIZE": MAX_STACK_SIZE}
    ops = re.findall('[+*/-/%]|\d+|\w+', line)
    for op in ops:
        # handle_op(stack, op, env)
        build_op_tree(stack, op, env)

    print(str(stack.pop()))
