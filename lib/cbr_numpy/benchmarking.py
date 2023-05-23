import ast
import csv
import nbformat as nbf
import numpy as np
import os
import subprocess
import sys
from  datasets  import  load_dataset
from lib.cbr_numpy.parser import parse
from timeit import default_timer as timer

# benchmark cells of a single .ipynb file
def benchmark_nb(in_filepath, out_filepath, build=True):
    # build dune executable
    if build:
        subprocess.run(['dune', 'build', 'benchmark/main.exe'])

    # parse jupyter notebook
    notebook = nbf.read(in_filepath, nbf.NO_CONVERT)
    all_stats = []

    for cell in notebook['cells']:
        if cell['cell_type'] == 'code':
            stats = benchmark_cell(cell)
            all_stats.append(stats)

    # fields for csv
    fields = ['cell name',
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
    
    # write to csv
    with open(out_filepath, 'r+', newline='') as file: 
        writer = csv.DictWriter(file, fieldnames = fields, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(all_stats)

def benchmark_cell(cell):
    code = cell['source']
    stats = {}
    stats['orig code'] = code

    # execute cell and eval last line
    try:
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
    
    # perform synthesis
    try:
        # set cell name
        if code[0] == '#':
            cell_name = code[1:code.find('\n')]
            stats['cell name'] = cell_name

        # convert last line to return statement
        last_ln_i = code.rfind('\n')
        code = code[:last_ln_i] + '\nreturn ' + code[last_ln_i + 1:]

        # parse synthesis target
        env, body = code.split('#synth')
        sexp = parse(body)

        # call synthesis from subprocess
        start = timer()
        synthed_body = subprocess.check_output('./_build/default/benchmark/main.exe', input=output_type + '\n' + str(sexp), text=True)
        end = timer()

        # add env back to synthesized code
        synthed = env + synthed_body

        stats['synthed code'] = synthed
        stats['synth time'] = end - start
    except:
        stats['status'] = 'SynthFail'
        return stats
    
    # execute synthesized code and eval last line
    try:
        synthed_ast = ast.parse(synthed, mode='exec')
        synthed_ast_size = num_nodes(synthed_ast)
        synthed_output, synthed_exec_time = exec_eval(synthed_ast)

        stats['synthed ast size'] = synthed_ast_size
        stats['synthed output'] = synthed_output
        stats['synthed exec time'] = synthed_exec_time

        if output_type == 'Number':
            stats['outputs match?'] = synthed_output == orig_output
        elif output_type == 'Array':
            stats['outputs match?'] = np.array_equal(synthed_output, orig_output)
    except:
        stats['status'] = 'SynthedExecFail'
        return stats
    
    stats['status'] = 'Success'
    return stats

def is_num(x):
    return isinstance(x, int) or isinstance(x, float) or type(x) == np.dtype(int) or type(x) == np.dtype(float)

def is_num_array(x):
    return isinstance(x, np.ndarray) and (x.dtype==np.dtype(int) or x.dtype==np.dtype(float))

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

    # assume that numpy is imported as np
    exec(compile("import numpy as np", '<string>', mode='exec'), _globals, _locals)

    # execute the cell and evaluate the last line
    start = timer()
    exec(compile(tree, '<string>', mode='exec'), _globals, _locals)
    output = eval(compile(last, '<string>', mode='eval'), _globals, _locals)
    end = timer()

    # return (output, execution time)
    return (output, end - start)

def main():
    sys.path.append("../..")
    dir = os.path.dirname(os.path.abspath(__file__))
    input_ipynb = os.path.join(dir, "data/benchmarking/targets.ipynb")
    output_csv = os.path.join(dir, "data/benchmarking/benchmarks.csv")

    benchmark_nb(input_ipynb, output_csv)

if __name__ == '__main__':
    main()