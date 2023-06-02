import ast
import csv
import contextlib
import nbformat as nbf
import numpy as np
import os
import subprocess
import sys
from lib.cbr_numpy.parser import parse
from lib.cbr_numpy.extractor import extract
from timeit import default_timer as timer

CSV_FIELDS = ['cell name',
              'orig code',
              'orig output',
              'orig ast size',
              'orig exec time',
              'synthed code',
              'synthed output',
              'synthed ast size',
              'synthed exec time',
              'synth time',
              'outputs match?',
              'status']

# benchmark cells of a single notebook


def benchmark_nb(notebook, build=True):
    # build dune executable
    if build:
        build_benchmark()
    all_stats = []

    for cell in notebook['cells']:
        if cell['cell_type'] == 'code':
            stats = benchmark_cell(cell)
            all_stats.append(stats)

    return all_stats

# benchmark one cell


def benchmark_cell(cell):
    if type(cell['source']) == str:
        code = cell['source']
    elif type(cell['source']) == list:
        code = '\n'.join(cell['source'])
    else:
        raise Exception('Cell source must be a string or a list')

    stats = {}
    stats['orig code'] = code.replace('\n', '\\n')[:10000]

    # set cell name
    if len(code) > 0 and code[0] == '#':
        cell_name = code[1:code.find('\n')]
        stats['cell name'] = cell_name

    # execute cell and eval last line
    '''
    try:
        if 'input(' in code:
            raise Exception('Cell cannot request user input')

        orig_ast = ast.parse(code, mode='exec')
        orig_ast_size = num_nodes(orig_ast)
        orig_output, orig_exec_time = exec_eval(orig_ast)

        stats['orig ast size'] = orig_ast_size
        stats['orig output'] = orig_output
        stats['orig exec time'] = orig_exec_time
    except:
        stats['status'] = 'OrigExecFail'
        return stats

    # output must be an int/float or a numpy array of ints/floats
    if is_num(orig_output):
        output_type = "Number"
    elif is_num_array(orig_output):
        output_type = "Array"
    else:
        stats['status'] = 'OutputTypeFail'
        return stats
    '''
    # perform synthesis
    for output_type in ["Number", "Array"]:
        try:

            synthed, synth_time = synthesize(code, output_type)

            stats['synthed code'] = synthed.replace('\n', '\\n')[:30000]
            stats['synth time'] = synth_time
            if synthed == "no solution found":
                stats['status'] = 'SynthFail'
            else:
                stats['status'] = 'Success'
        except:
            stats['status'] = 'SynthFail'
            # return stats
    '''
    # execute synthesized code and eval last line
    try:
        synthed_ast = ast.parse(synthed, mode='exec')
        synthed_ast_size = num_nodes(synthed_ast)
        synthed_output, synthed_exec_time = exec_eval(synthed_ast)

        stats['synthed ast size'] = synthed_ast_size
        stats['synthed output'] = synthed_output
        stats['synthed exec time'] = synthed_exec_time
    except:
        stats['status'] = 'SynthedExecFail'
        return stats

    if output_type == 'Number':
        stats['outputs match?'] = synthed_output == orig_output
    elif output_type == 'Array':
        stats['outputs match?'] = np.array_equal(synthed_output, orig_output)
    '''

    return stats


def build_benchmark():
    subprocess.run(['dune', 'build', 'benchmark_np/main.exe'])


def is_num(x):
    return isinstance(x, int) or isinstance(x, float) or type(x) == np.dtype(int) or type(x) == np.dtype(float)


def is_num_array(x):
    return isinstance(x, np.ndarray) and (x.dtype == np.dtype(int) or x.dtype == np.dtype(float))

# NodeVisitor that counts the total number of nodes in an AST


class NodeCounter(ast.NodeVisitor):
    def __init__(self):
        self.count = 0

    def generic_visit(self, node):
        self.count += 1
        ast.NodeVisitor.generic_visit(self, node)

# compute number of nodes in an AST


def num_nodes(tree):
    counter = NodeCounter()
    counter.visit(tree)
    return counter.count

# execute a Python AST and eval the last line


def exec_eval(tree):
    last = ast.Expression(tree.body.pop().value)
    _globals, _locals = {}, {}

    # disable IO
    with open(os.devnull, "w") as f, contextlib.redirect_stdout(f):
        # assume that numpy is imported as np
        exec(compile("import numpy as np", '<string>',
             mode='exec'), _globals, _locals)

        # execute the cell and evaluate the last line
        start = timer()
        exec(compile(tree, '<string>', mode='exec'), _globals, _locals)
        output = eval(compile(last, '<string>', mode='eval'),
                      _globals, _locals)
        end = timer()

    # return (output, execution time)
    return output, end - start

# preproccess code and run synthesis


def synthesize(code, output_type="Number"):
    # convert last line to return statement
    last_ln_i = code.rfind('\n')
    code = code[:last_ln_i] + '\nreturn ' + code[last_ln_i + 1:]

    # parse synthesis target
    env, body = extract(code)
    sexp = parse(body)
    # call synthesis from subprocess
    start = timer()
    synthed_body = subprocess.check_output(
        './_build/default/benchmark_np/main.exe', input=output_type + '\n' + str(sexp), text=True)
    end = timer()
    print('test')
    # add env back to synthesized code
    if "no solution found" in synthed_body:
        synthed = "no solution found"
    else:
        synthed = ast.unparse(env) + '\n' + synthed_body

    # return (synthesis output, synthesis time)
    return synthed, end - start

# benchmark tests in data/benchmarking/targets.ipynb


def main():
    sys.path.append("../..")
    dir = os.path.dirname(os.path.abspath(__file__))
    input_ipynb = os.path.join(dir, "data/benchmarking/targets.ipynb")
    output_csv = os.path.join(dir, "data/benchmarking/benchmarks.csv")

    # read .ipynb file
    notebook = nbf.read(input_ipynb, nbf.NO_CONVERT)

    all_stats = benchmark_nb(notebook)

    # write to csv
    with open(output_csv, 'w', newline='') as file:
        file.truncate()
        writer = csv.DictWriter(
            file, fieldnames=CSV_FIELDS, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(all_stats)


if __name__ == '__main__':
    main()
