import ast
import json
import subprocess

from timeit import default_timer as timer

import python_to_ir
import extract
import util

OCAML_TIME_FIELDS = [
    "ocaml_synthesis_time",
    "ocaml_canonicalization_time",
    "ocaml_unification_time",
]


CSV_FIELDS = [
    "orig code",
    "synthed code",
    "synth time",
    "status",
    "reason",
    "orig output",
    "orig ast size",
    "orig exec time",
    "synthed output",
    "synthed ast size",
    "synthed exec time",
    "exec status",
    "exec reason",
    "kind",
] + OCAML_TIME_FIELDS


def elm_json(
    js, dry_run=False, toggle_eval=False
):  # TODO: deal with toggle_eval somehow
    """Runs a single Elm function/variable definition, assuming that it is
    represented as an elm-format JSON object"""
    stats = {}
    stats["orig code"] = util.csv_str_encode(json.dumps(js))

    try:
        block, kind = extract.elm_json(js)
    except extract.NoExtractionException:
        stats["status"] = "ExtractFail"
        return stats

    stats["kind"] = kind

    if dry_run:
        synthesis_result = {"status": "DryRun"}
    else:
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

    for key in OCAML_TIME_FIELDS:
        if key in synthesis_result:
            stats[key] = synthesis_result[key]

    if synthesis_result["status"] == "Success":
        stats["synthed code"] = util.csv_str_encode(synthesis_result["solution"])
        stats["synthed ast size"] = synthesis_result["size"]

    return stats


def python_helper(tree, dry_run=False, rewrite_for=None, toggle_eval=False):
    """Runs a Python script, assuming that it is represented as a Python AST
    object"""
    assert rewrite_for is not None

    stats = {}
    stats["orig code"] = util.csv_str_encode(ast.unparse(tree))

    try:
        pre, body, post, output_variable = extract.python(tree, rewrite_for=rewrite_for)
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

    if dry_run:
        synthesis_result = {"status": "DryRun"}
    else:
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

    for key in OCAML_TIME_FIELDS:
        if key in synthesis_result:
            stats[key] = synthesis_result[key]

    if synthesis_result["status"] == "Success":
        synthed_code = (
            ast.unparse(pre)
            + f"\n{output_variable} = "
            + synthesis_result["solution"]
            + "\n"
            + ast.unparse(post)
        ).strip()

        stats["synthed code"] = util.csv_str_encode(synthed_code)
        stats["synthed ast size"] = synthesis_result["size"]

        if toggle_eval:
            try:
                orig_ast_size = util.num_nodes(tree)
                orig_output, orig_exec_time = util.exec_eval(tree)

                stats["orig ast size"] = orig_ast_size
                stats["orig output"] = orig_output
                stats["orig exec time"] = orig_exec_time
            except Exception as e:
                stats["exec status"] = "OrigExecFail"
                stats["exec reason"] = repr(e)
                return stats

            try:
                synthed_ast = ast.parse(synthed_code)
                synthed_output, synthed_exec_time = util.exec_eval(synthed_ast)

                stats["synthed output"] = synthed_output
                stats["synthed exec time"] = synthed_exec_time
            except Exception as e:
                stats["exec status"] = "SynthedExecFail"
                stats["exec reason"] = repr(e)
                return stats

            if orig_output == synthed_output:
                stats["exec status"] = "OutputMatch"
            else:
                stats["exec status"] = "OutputMismatch"
    return stats


def python(tree, dry_run=False, toggle_eval=False):
    """Runs a Python script, assuming that it is represented as a Python AST
    object (tries not rewriting and rewriting the for statement)"""
    stats = python_helper(
        tree, dry_run=dry_run, rewrite_for=False, toggle_eval=toggle_eval
    )
    if stats["status"] != "Success":
        stats2 = python_helper(
            tree, dry_run=dry_run, rewrite_for=True, toggle_eval=toggle_eval
        )
        if "synth time" in stats2 and "synth time" in stats:
            stats2["synth time"] += stats["synth time"]
        stats = stats2
    return stats
