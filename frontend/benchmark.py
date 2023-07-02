import ast
import json
import subprocess

from timeit import default_timer as timer

import python_to_ir
import extract
import util


CSV_FIELDS = [
    "orig code",
    "synthed code",
    "synth time",
    "synth status",
    "synth reason",
    "orig output",
    "orig ast size",
    "orig exec time",
    "synthed output",
    "synthed ast size",
    "synthed exec time",
    "exec status",
    "exec reason",
]


def elm_json(js):
    """Benchmarks a single Elm function/variable definition, assuming that it is
    represented as an elm-format JSON object"""
    stats = {}
    stats["orig code"] = util.csv_str_encode(json.dumps(js))
    stats["exec status"] = len(stats["orig code"])

    try:
        block = extract.elm_json(js)
    except extract.NoExtractionException:
        stats["synth status"] = "ExtractFail"
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

    stats["synth status"] = synthesis_result["status"]
    if "reason" in synthesis_result:
        stats["synth reason"] = synthesis_result["reason"]

    if synthesis_result["synth status"] == "Success":
        stats["synthed code"] = util.csv_str_encode(synthesis_result["solution"])

    return stats


def python(cell_code, code_so_far, toggle_eval=False):
    """Benchmarks a Python script, assuming that it is represented as a Python
    AST object"""
    stats = {}
    stats["orig code"] = util.csv_str_encode(cell_code)
    
    try:
        parsed_cell = ast.parse(cell_code)
    except Exception as e:
        stats["synth status"] = "ParseFail"
        stats["synth reason"] = repr(e)
        return stats

    try:
        pre, body, post, output_variable = extract.python(parsed_cell)
    except extract.NoExtractionException as e:
        stats["synth status"] = "ExtractFail"
        stats["synth reason"] = repr(e)
        return stats

    try:
        sexp = python_to_ir.parse(body)
    except Exception as e:
        stats["synth status"] = "IRConversionFail"
        stats["synth reason"] = repr(e)
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

    stats["synth status"] = synthesis_result["status"]
    if "reason" in synthesis_result:
        stats["synth reason"] = synthesis_result["reason"]

    if synthesis_result["status"] == "Success":
        synthed_code = (
            ast.unparse(pre)
            + f"\n{output_variable} = "
            + synthesis_result["solution"]
            + "\n"
            + ast.unparse(post)
        ).strip()

        stats["synthed code"] = util.csv_str_encode(synthed_code)

        if toggle_eval:
            try:
                #orig_ast = ast.parse(code_so_far + "\n" + cell_code, mode="exec")
                orig_ast = ast.parse(cell_code, mode="exec")
                orig_ast_size = util.num_nodes(orig_ast)
                orig_output, orig_exec_time = util.exec_eval(orig_ast)

                stats["orig ast size"] = orig_ast_size
                stats["orig output"] = orig_output
                stats["orig exec time"] = orig_exec_time
            except Exception as e:
                stats["exec status"] = "OrigExecFail"
                stats["exec reason"] = repr(e)
                return stats
            
            try:
                #synthed_ast = ast.parse(code_so_far + "\n" + synthed_code, mode="exec")
                synthed_ast = ast.parse(synthed_code, mode="exec")
                synthed_ast_size = util.num_nodes(synthed_ast)
                synthed_output, synthed_exec_time = util.exec_eval(synthed_ast)

                stats["synthed ast size"] = synthed_ast_size
                stats["synthed output"] = synthed_output
                stats["synthed exec time"] = synthed_exec_time
            except:
                stats["exec status"] = "SynthedExecFail"
                stats["exec reason"] = repr(e)
                return stats
            
            stats["exec status"] = orig_output == synthed_output

    return stats