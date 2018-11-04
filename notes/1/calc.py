#!/usr/bin/env python3

from sys import stdin
import fileinput
import operator

ENV = {}

ADD = '+'
MUL = '*'
SUB = '-'
DIV = '/'
MOD = '%'
WRITE = '.'
SPACE = ' '

OPS = [ADD, MUL, SUB, DIV, MOD, WRITE, COLON]

SIGN_TO_FUN = {
    ADD: (operator.add, 2),
    MUL: (operator.mul, 2),
    SUB: (operator.sub, 2),
    DIV: (operator.div, 2),
    MOD: (operator.mod, 2),
    WRITE: (print, 1),
    LOAD: ((lambda id: ENV[id]), 1),
    STORE: ((lambda val, id: ENV.__setitem__(id, val)), 2),
    COLON: ((lambda: pass), 0),
}


def tokenize(contents):
    input = []
    expr = []

    for letter in contents:
        if letter in OPS:
            input.append(SIGN_TO_FUN(letter))
        elif letter.isdigit():
            
        elif letter.isalpha():
            pass
        elif letter == SPACE:
            if expr:
                if all(x.isdigit() for x in expr):
                    input.append(int(expr))
                elif all(x.isalpha() for x in expr):
                    input.append(expr)
                else raise ValueError
                expr = []
        else raise ValueError


input = "".join([line.translate(str.maketrans(dict.fromkeys(' \n\t\r'))) for line in fileinput.input()])
execute(tokenize(input))

