import ast
import json
import subprocess

from timeit import default_timer as timer

import python_to_ir
import extract
import util


CSV_FIELDS = [
    "orig code",  #
    "orig output",
    "orig ast size",
    "orig exec time",
    "synthed code",  #
    "synthed output",
    "synthed ast size",
    "synthed exec time",
    "synth time",  #
    "outputs match?",
    "status",  #
    "reason",  #
]


def elm_json(js):
    """Benchmarks a single Elm function/variable definition, assuming that it is
    represented as an elm-format JSON object"""
    stats = {}
    stats["orig code"] = util.csv_str_encode(json.dumps(js))
    stats["outputs match?"] = len(stats["orig code"])

    try:
        block = extract.elm_json(js)
    except extract.NoExtractionException:
        stats["status"] = "ExtractFail"
        return stats

    start = timer()

    synthesis_result = subprocess.check_output(
        [util.path_from_root("backend/_build/default/bin/main.exe"), "elm"],
        input=json.dumps(block),
        text=True,
    )

    end = timer()

    stats["synth time"] = end - start

    synthesis_result = json.loads(synthesis_result)

    stats["status"] = synthesis_result["status"]
    if "reason" in synthesis_result:
        stats["reason"] = synthesis_result["reason"]

    if synthesis_result["status"] == "Success":
        stats["synthed code"] = util.csv_str_encode(synthesis_result["solution"])

    return stats


def python(tree):
    """Benchmarks a Python script, assuming that it is represented as a Python
    AST object"""
    stats = {}
    stats["orig code"] = util.csv_str_encode(ast.unparse(tree))

    try:
        env, body, output_variable = extract.python(tree)
        # stats["status"] = "TODO Extracted!"
        # stats["synthed code"] = util.csv_str_encode(
        #     "# CBR env\n" + ast.unparse(env) + "\n# CBR body\n" + ast.unparse(body)
        # )
        # return stats
    except extract.NoExtractionException as e:
        stats["status"] = "ExtractFail"
        stats["reason"] = repr(e)
        return stats

    try:
        sexp = python_to_ir.parse(body)
    except Exception as e:
        stats["status"] = "IRConversionFail"
        stats["reason"] = repr(e)
        return stats

    start = timer()
    synthesis_result = subprocess.check_output(
        [util.path_from_root("backend/_build/default/bin/main.exe"), "python"],
        input=str(sexp),
        text=True,
    )
    end = timer()

    stats["synth time"] = end - start

    synthesis_result = json.loads(synthesis_result)

    stats["status"] = synthesis_result["status"]
    if "reason" in synthesis_result:
        stats["reason"] = synthesis_result["reason"]

    if synthesis_result["status"] == "Success":
        stats["synthed code"] = util.csv_str_encode(
            (
                ast.unparse(env)
                + f"\n{output_variable} = "
                + synthesis_result["solution"]
            ).strip()
        )

    return stats


# # execute cell and eval last line
# try:
#     if 'input(' in code:
#         raise Exception('Cell cannot request user input')

#     orig_ast = ast.parse(code, mode='exec')
#     orig_ast_size = num_nodes(orig_ast)
#     orig_output, orig_exec_time = exec_eval(orig_ast)

#     stats['orig ast size'] = orig_ast_size
#     stats['orig output'] = orig_output
#     stats['orig exec time'] = orig_exec_time
# except:
#     stats['status'] = 'OrigExecFail'
#     return stats

# # output must be an int/float or a numpy array of ints/floats
# if is_num(orig_output):
#     output_type = "Number"
# elif is_num_array(orig_output):
#     output_type = "Array"
# else:
#     stats['status'] = 'OutputTypeFail'
#     return stats

# # execute synthesized code and eval last line
# try:
#     synthed_ast = ast.parse(synthed, mode='exec')
#     synthed_ast_size = num_nodes(synthed_ast)
#     synthed_output, synthed_exec_time = exec_eval(synthed_ast)

#     stats['synthed ast size'] = synthed_ast_size
#     stats['synthed output'] = synthed_output
#     stats['synthed exec time'] = synthed_exec_time
# except:
#     stats['status'] = 'SynthedExecFail'
#     return stats

# if output_type == 'Number':
#     stats['outputs match?'] = synthed_output == orig_output
# elif output_type == 'Array':
#     stats['outputs match?'] = np.array_equal(synthed_output, orig_output)
