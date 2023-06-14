import ast
import subprocess

from timeit import default_timer as timer

import python_to_ir
import extract
import util


CSV_FIELDS = [
    "orig code",
    "orig output",
    "orig ast size",
    "orig exec time",
    "synthed code",
    "synthed output",
    "synthed ast size",
    "synthed exec time",
    "synth time",
    "outputs match?",
    "status",
]


def elm_json(ex):
    """Benchmarks a single Elm function/variable definition, assuming that it is
    represented as an elm-format JSON object"""
    stats = {}
    try:
        json_str = extract.elm_json(ex)
        try:
            start = timer()
            stats["orig code"] = json_str
            output = subprocess.check_output(
                util.path_from_root("backend/_build/default/benchmark_fp/main.exe"),
                input=json_str,
                stderr=subprocess.PIPE,
                text=True,
            )
            end = timer()
            stats["synthed code"] = output
            stats["synth time"] = end - start
            stats["status"] = "Success_TODO"
        except subprocess.CalledProcessError as e:
            err = e.stderr
            if "Yojson" in err or "unknown" in err:
                stats["status"] = "ParseFail_TODO"
            else:
                stats["status"] = "SynthFail_TODO"
    except extract.NoSynthException:
        stats["status"] = "ExtractFail"
    return stats


def python(tree):
    """Benchmarks a Python script, assuming that it is represented as a Python
    AST object"""
    stats = {}
    stats["orig code"] = ast.dump(tree).replace("\n", "_CBR__NL___").replace("\t", "    ")

    # execute cell and eval last line
    """
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
    """

    # perform synthesis
    for output_type in ["Number", "Array"]:
        try:
            # convert last line to return statement
            last_ln_i = code.rfind("\n")
            code = code[:last_ln_i] + "\nreturn " + code[last_ln_i + 1 :]

            # parse synthesis target
            tree = ast.parse(code)
            env, body = extract.python(tree)

            try:
                sexp = python_to_ir.parse(body)
                # call synthesis from subprocess
                start = timer()
                synthed_body = subprocess.check_output(
                    util.path_from_root("backend/_build/default/benchmark_np/main.exe"),
                    input=output_type + "\n" + str(sexp),
                    text=True,
                )
                end = timer()

                # add env back to synthesized code
                if "no solution found" in synthed_body:
                    synthed = "no solution found"
                else:
                    synthed = ast.unparse(env) + "\n" + synthed_body

                stats["synthed code"] = synthed.replace("\n", "\\n")[:30000]
                stats["synth time"] = end - start
                if synthed == "no solution found":
                    stats["status"] = "SynthFail"
                else:
                    stats["status"] = "Success"
            except:
                stats["status"] = "ParseFail"
        except extract.NoSynthException:
            stats["status"] = "ExtractFail"

    """
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

    """
    return stats


def python_cell(cell):
    """Benchmarks a single Jupyter notebook cell, assuming that it is
    represented as a JSON object"""
    if type(cell["source"]) == str:
        return python(cell["source"])
    elif type(cell["source"]) == list:
        return python("\n".join(cell["source"]))
    else:
        raise Exception("Cell source must be a string or a list")
