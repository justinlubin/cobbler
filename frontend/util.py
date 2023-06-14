import ast
import contextlib
import os
from timeit import default_timer as timer
import numpy as np
import sys


def path_from_root(path):
    """Returns a path from the root of the project"""
    return os.path.normpath(os.path.join(os.path.dirname(sys.path[0]), path))


def exec_eval(tree):
    """Executes a Python AST and evals the last line"""
    last = ast.Expression(tree.body.pop().value)
    _globals, _locals = {}, {}

    # disable IO
    with open(os.devnull, "w") as f, contextlib.redirect_stdout(f):
        # assume that numpy is imported as np
        exec(compile("import numpy as np", "<string>", mode="exec"), _globals, _locals)

        # execute the cell and evaluate the last line
        start = timer()
        exec(compile(tree, "<string>", mode="exec"), _globals, _locals)
        output = eval(compile(last, "<string>", mode="eval"), _globals, _locals)
        end = timer()

    # return (output, execution time)
    return output, end - start


def is_num(x):
    return isinstance(x, int) or isinstance(x, float) or type(x) == np.dtype(int) or type(x) == np.dtype(float)


def is_num_array(x):
    return isinstance(x, np.ndarray) and (x.dtype == np.dtype(int) or x.dtype == np.dtype(float))


class NodeCounter(ast.NodeVisitor):
    """NodeVisitor that counts the total number of nodes in an AST"""

    def __init__(self):
        self.count = 0

    def generic_visit(self, node):
        self.count += 1
        ast.NodeVisitor.generic_visit(self, node)


def num_nodes(tree):
    """Counts the total number of nodes in an AST"""
    counter = NodeCounter()
    counter.visit(tree)
    return counter.count
