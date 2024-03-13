#!/usr/bin/env python3

import argparse
import ast
import csv
import json
import pathlib
import subprocess
import sys
import os

import run_backend
import db_iter
import util

import numpy as np


def use_quick_eval():
    return (
        "COBBLER_QUICK_EVAL" in os.environ and os.environ["COBBLER_QUICK_EVAL"] == "1"
    )


BENCHMARK_REPLICATES = 2 if use_quick_eval() else 10


def refresh_binary():
    try:
        subprocess.run(
            ["make", "regen-stdlib"],
            cwd=util.path_from_root("backend"),
            check=True,
            stdout=open(os.devnull, "wb"),
        )
    except subprocess.CalledProcessError:
        sys.exit(1)

    try:
        subprocess.run(
            ["make", "build"],
            cwd=util.path_from_root("backend"),
            check=True,
            stdout=open(os.devnull, "wb"),
        )
    except subprocess.CalledProcessError:
        sys.exit(1)


def show_elm(s):
    try:
        return "\n".join(
            subprocess.check_output(
                ["elm-format", "--stdin"],
                input=s.encode("utf8"),
                stderr=subprocess.PIPE,
            )
            .decode("utf8")
            .splitlines()[3:]
        )
    except subprocess.CalledProcessError as e:
        return "elm-format error: " + e.stderr.decode("utf8") + "\n\n" + s


def prettify_elm_json(js):
    prettify_output = subprocess.check_output(
        [util.path_from_root("backend/_build/default/bin/main.exe"), "elm-prettify"],
        input=js,
        text=True,
    )
    return show_elm(json.loads(prettify_output))


def prettify_elm(code):
    elm_format_output = subprocess.check_output(
        ["elm-format", "--stdin", "--json"],
        input=code.encode("utf-8"),
    )
    prettify_output = subprocess.check_output(
        [util.path_from_root("backend/_build/default/bin/main.exe"), "elm-prettify"],
        input=json.dumps(json.loads(elm_format_output)["body"][0]),
        text=True,
    )
    return show_elm(json.loads(prettify_output))


def show_python(s):
    return s


def refactor_helper(language=None, show_input=None):
    code = sys.stdin.read()

    if language == "elm":
        elm_format_output = subprocess.check_output(
            ["elm-format", "--stdin", "--json"],
            input=code.encode("utf-8"),
        )
        stats = run_backend.elm_json(json.loads(elm_format_output)["body"][0])
    elif language == "python":
        stats = run_backend.python(ast.parse(code))

    if stats["status"] == "Success":
        decoded_data = util.csv_str_decode(stats["synthed code"])
        if language == "elm":
            if show_input:
                print(prettify_elm(code))
                print(";" * 80)
            print(show_elm(decoded_data))
        elif language == "python":
            if show_input:
                print(show_python(code))
                print(";" * 80)
            print(show_python(decoded_data))
    else:
        print("No solution found.")
        print("Status:", stats["status"])
        print("Reason:", stats["reason"] if "reason" in stats else "")
        sys.exit(1)


def run_many_helper(
    path=None,
    generator=None,
    runner=None,
    sample_limit=100,
    dry_run=None,
):
    with open(path, "w", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=run_backend.CSV_FIELDS,
            delimiter="\t",
        )
        writer.writeheader()
        previous_path = None
        sample_num = 0
        for generator_output in generator(sample_limit=sample_limit):
            sample_path, block = generator_output
            if sample_path != previous_path:
                if previous_path is not None:
                    sample_num += 1
                    print(f"Completed '{previous_path}' ({sample_num}/{sample_limit})")
                previous_path = sample_path
            stats = runner(block, dry_run=dry_run)
            if stats["status"] in ["Success", "SynthFail"]:
                times = []
                ocaml_times = {key: [] for key in run_backend.OCAML_TIME_FIELDS}
                for _ in range(BENCHMARK_REPLICATES):
                    stats_tmp = runner(block, dry_run=dry_run)
                    assert stats_tmp["status"] == stats["status"]
                    times.append(str(stats_tmp["synth time"]))
                    for key in run_backend.OCAML_TIME_FIELDS:
                        ocaml_times[key].append(str(stats_tmp[key]))
                stats["synth time"] = ",".join(times)
                for key in run_backend.OCAML_TIME_FIELDS:
                    stats[key] = ",".join(ocaml_times[key])
            writer.writerow(stats)
        print(f"Completed '{previous_path}' ({sample_num+1}/{sample_limit})")


def view_result_helper(
    path=None,
    line_number=None,
    show_code=None,
    show_synthed_code=None,
):
    with open(path, "r", newline="") as f:
        for i, row in enumerate(csv.DictReader(f, delimiter="\t")):
            if i == line_number - 2:
                print("status:", row["status"])
                print("reason:", row["reason"])
                print("synth time:", row["synth time"])
                print("-" * 80)
                print(
                    "orig code:",
                    show_code(util.csv_str_decode(row["orig code"])),
                    sep="\n",
                )
                print("-" * 80)
                print(
                    "synthed code:",
                    show_synthed_code(util.csv_str_decode(row["synthed code"])),
                    sep="\n",
                )
                print("exec status:", row["exec status"])
                print("exec reason:", row["exec reason"])
                break


def make_report_helper(
    input_path=None,
    output_path=None,
    language=None,
):
    show_code = prettify_elm_json if language == "elm" else show_python
    show_synthed_code = prettify_elm if language == "elm" else show_python
    comment = "--" if language == "elm" else "##"
    with open(input_path, "r", newline="") as input_f:
        with open(output_path, "w") as output_f:
            for row in csv.DictReader(input_f, delimiter="\t"):
                output_f.write(comment + " " + "=" * 80 + "\n")
                output_f.write(comment + " Synthesis status: " + row["status"] + "\n")
                if row["reason"]:
                    output_f.write(comment + " Reason: " + row["reason"] + "\n")
                output_f.write(
                    comment + " Synthesis time: " + row["synth time"] + "\n\n"
                )
                output_f.write(comment + " Original code:\n\n")
                try:
                    output_f.write(
                        show_code(util.csv_str_decode(row["orig code"])) + "\n\n"
                    )
                except:
                    pass
                try:
                    if row["synthed code"]:
                        output_f.write(comment + " Synthesized code:\n\n")
                        output_f.write(
                            show_synthed_code(util.csv_str_decode(row["synthed code"]))
                            + "\n\n"
                        )
                except:
                    pass
                if row["exec status"]:
                    output_f.write(
                        comment + " Execution status: " + row["exec status"] + "\n"
                    )
                if row["exec reason"]:
                    output_f.write(comment + " Reason: " + row["exec reason"] + "\n")


def filter_helper(
    input_path=None,
    output_path=None,
    invert=None,
    statuses=None,
):
    with open(input_path, "r", newline="") as input_f:
        with open(output_path, "w", newline="") as output_f:
            writer = csv.DictWriter(
                output_f,
                fieldnames=run_backend.CSV_FIELDS,
                delimiter="\t",
            )
            writer.writeheader()
            for row in csv.DictReader(input_f, delimiter="\t"):
                in_statuses = row["status"] in statuses
                if not invert and in_statuses or invert and not in_statuses:
                    writer.writerow(row)


def subtract_helper(
    superset_path=None,
    subset_path=None,
    output_path=None,
):
    seen_subset = set()
    with open(subset_path, "r", newline="") as subset_f:
        for row in csv.DictReader(subset_f, delimiter="\t"):
            seen_subset.add(row["orig code"])

    with open(superset_path, "r", newline="") as superset_f:
        with open(output_path, "w", newline="") as output_f:
            writer = csv.DictWriter(
                output_f,
                fieldnames=run_backend.CSV_FIELDS,
                delimiter="\t",
            )
            writer.writeheader()
            for row in csv.DictReader(superset_f, delimiter="\t"):
                if row["orig code"] in seen_subset:
                    continue
                writer.writerow(row)


def rerun_many_helper(
    input_path=None,
    output_path=None,
    language=None,
    depth=None,
    timing_breakdown=None,
    ablation=None,
):
    if language == "elm":
        runner = lambda s, **kwargs: run_backend.elm_json(
            json.loads(s),
            depth=depth,
            timing_breakdown=timing_breakdown,
            ablation=ablation,
            **kwargs,
        )
    elif language == "python":
        runner = lambda s, **kwargs: run_backend.python(
            ast.parse(s),
            depth=depth,
            timing_breakdown=timing_breakdown,
            ablation=ablation,
            **kwargs,
        )

    def generator(sample_limit=None):
        with open(input_path, "r", newline="") as input_f:
            for i, row in enumerate(csv.DictReader(input_f, delimiter="\t")):
                yield f"Row {i + 2}", util.csv_str_decode(row["orig code"])

    run_many_helper(
        path=output_path,
        generator=generator,
        runner=runner,
        sample_limit=None,
    )


def summarize_helper(path=None):
    summary = {}
    total = 0

    with open(path, "r", newline="") as f:
        for row in csv.DictReader(f, delimiter="\t"):
            status = row["status"]
            if status in summary:
                summary[status] += 1
            else:
                summary[status] = 1
            total += 1

    for status in sorted(summary, key=summary.get, reverse=True):
        print(
            f"{status}: {summary[status]} ({round(100 * summary[status] / total, 2)}%)"
        )
    print(f"Total: {total}")


def check_no_worse_helper(before_path=None, after_path=None):
    before_statuses = []
    after_statuses = []

    with open(before_path, "r", newline="") as f:
        for row in csv.DictReader(f, delimiter="\t"):
            before_statuses.append(row["status"])
    with open(after_path, "r", newline="") as f:
        for row in csv.DictReader(f, delimiter="\t"):
            after_statuses.append(row["status"])

    if len(before_statuses) != len(after_statuses):
        print('"before" and "after" TSVs have unequal lengths')
        sys.exit(1)

    row_failures = []

    for i, (before_status, after_status) in enumerate(
        zip(before_statuses, after_statuses)
    ):
        if before_status == "Success" and after_status != "Success":
            row_failures.append(str(i + 2))

    if row_failures != []:
        print("[ERROR] The following rows got worse:", ", ".join(row_failures))
        sys.exit(1)
    else:
        print("All good!")


def remove_duplicates_helper(
    input_path=None,
    output_path=None,
):
    seen = set()
    with open(input_path, "r", newline="") as input_f:
        with open(output_path, "w", newline="") as output_f:
            writer = csv.DictWriter(
                output_f,
                fieldnames=run_backend.CSV_FIELDS,
                delimiter="\t",
            )
            writer.writeheader()
            for row in csv.DictReader(input_f, delimiter="\t"):
                orig_code = row["orig code"]
                if orig_code in seen:
                    continue
                writer.writerow(row)
                seen.add(orig_code)


def gen_survey_code_helper(
    input_path=None,
    output_path=None,
    language=None,
):
    show_code = prettify_elm_json if language == "elm" else show_python
    show_synthed_code = show_elm if language == "elm" else show_python
    extension = "elm" if language == "elm" else "py"
    with open(input_path, "r", newline="") as input_f:
        for i, row in enumerate(csv.DictReader(input_f, delimiter="\t")):
            if row["status"] != "Success" or int(row["synthed ast size"]) < 2:
                continue

            kind = row["kind"] if "kind" in row else "UnknownKind"

            row_number = i + 2
            folder = f"{output_path}/{kind}/row{row_number:06d}"
            os.makedirs(folder, exist_ok=True)

            with open(f"{folder}/input.{extension}", "w") as output_orig_f:
                with open(f"{folder}/output.{extension}", "w") as output_synthed_f:
                    output_orig_f.write(
                        show_code(util.csv_str_decode(row["orig code"])) + "\n\n"
                    )

                    output_synthed_f.write(
                        show_synthed_code(util.csv_str_decode(row["synthed code"]))
                    )


if __name__ == "__main__":
    if len(sys.argv) == 1:
        print(
            """                    ___       ___       ___
                   (   )     (   )     (   )
  .--.      .--.    | |.-.    | |.-.    | |    .--.    ___ .-.
 /    \    /    \   | /   \   | /   \   | |   /    \  (   )   \\
|  .-. ;  |  .-. ;  |  .-. |  |  .-. |  | |  |  .-. ;  | ' .-. ;
|  |(___) | |  | |  | |  | |  | |  | |  | |  |  | | |  |  / (___)
|  |      | |  | |  | |  | |  | |  | |  | |  |  |/  |  | |
|  | ___  | |  | |  | |  | |  | |  | |  | |  |  ' _.'  | |
|  '(   ) | '  | |  | '  | |  | '  | |  | |  |  .'.-.  | |
'  `-' |  '  `-' /  ' `-' ;   ' `-' ;   | |  '  `-' /  | |
 `.__,'    `.__.'    `.__.     `.__.   (___)  `.__.'  (___)"""
        )
        print("\ncobbler: the component-based refactoring synthesizer.\n")
        print(f"For help: {sys.argv[0]} --help")
        sys.exit(0)

    parser = argparse.ArgumentParser(
        description="hint: try cobbler out with\n  cat FILE.{elm,py} | ./cobbler refactor --language={elm,python}",
        usage="./cobbler [--help] SUBCOMMAND ...",
        formatter_class=argparse.RawTextHelpFormatter,
    )

    subparsers = parser.add_subparsers(
        dest="subcommand",
        metavar="SUBCOMMAND",
        help="for more help, run ./cobbler SUBCOMMAND --help",
        required=True,
    )

    ###

    check_no_worse_parser = subparsers.add_parser(
        "check-no-worse",
        help="ensure that a set of results is no worse than a previous one",
    )
    check_no_worse_parser.add_argument(
        "previous_results",
        type=pathlib.Path,
        help="the path to the previous results (in tsv format)",
    )
    check_no_worse_parser.add_argument(
        "new_results",
        type=pathlib.Path,
        help="the path to the new results (in tsv format)",
    )

    ###

    remove_duplicates_parser = subparsers.add_parser(
        "deduplicate",
        help="remove duplicates in a set of results",
    )
    remove_duplicates_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path to the results (in tsv format)",
    )
    remove_duplicates_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the new results (in tsv format)",
    )

    ###

    download_parser = subparsers.add_parser(
        "download",
        help="download data from The Stack",
    )
    download_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language to download",
    )
    download_parser.add_argument(
        "--sample-limit",
        type=int,
        default=20,
        help="the maximum number of samples (files) to draw from the database (default: 20)",
    )
    download_parser.add_argument(
        "path",
        type=pathlib.Path,
        help="the path download the data to (in tsv format)",
    )

    ###

    filter_parser = subparsers.add_parser(
        "filter",
        help="filter a set of results by status",
    )
    filter_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path to the results to filter (in tsv format)",
    )
    filter_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the filtered results (in tsv format)",
    )
    filter_parser.add_argument(
        "--invert",
        action=argparse.BooleanOptionalAction,
        help="exclude given statuses rather than include",
    )
    filter_parser.add_argument(
        "statuses",
        type=str,
        nargs="+",
        help="the statuses to filter",
    )

    ###

    gen_survey_code_parser = subparsers.add_parser(
        "gen-survey-code",
        help="generate code files for the survey from a set of results",
    )
    gen_survey_code_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the results",
    )
    gen_survey_code_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path to the set of results (in tsv format)",
    )
    gen_survey_code_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to the directory that should contain the output",
    )

    ###

    make_report_parser = subparsers.add_parser(
        "make-report",
        help="make a human-readable report of a set of results",
    )
    make_report_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the results",
    )
    make_report_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path to the set of results (in tsv format)",
    )
    make_report_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the report (in .elm or .py format)",
    )

    ###

    refactor_parser = subparsers.add_parser(
        "refactor",
        help="refactor code from stdin, printing to stdout",
    )
    refactor_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the code to refactor",
    )
    refactor_parser.add_argument(
        "--prettify-input",
        action=argparse.BooleanOptionalAction,
        help="also prettify (and print) the input",
    )

    ###

    refactor_many_parser = subparsers.add_parser(
        "refactor-many",
        help="run the synthesizer on a set of input programs",
    )
    refactor_many_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the input programs",
    )
    refactor_many_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path to the set of input programs (in tsv format)",
    )
    refactor_many_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the results (in tsv format)",
    )
    refactor_many_parser.add_argument(
        "--depth",
        type=int,
        required=False,
        help="how deep to search for solutions",
    )
    refactor_many_parser.add_argument(
        "--timing-breakdown",
        action=argparse.BooleanOptionalAction,
        help="also track timing breakdown",
    )
    refactor_many_parser.add_argument(
        "--ablation",
        action=argparse.BooleanOptionalAction,
        help="only perform syntactic unification",
    )

    ###

    subtract_parser = subparsers.add_parser(
        "subtract",
        help="remove entries from a set of results",
    )
    subtract_parser.add_argument(
        "--superset",
        type=pathlib.Path,
        required=True,
        help="the path to the superset of results (in tsv format)",
    )
    subtract_parser.add_argument(
        "--subset",
        type=pathlib.Path,
        required=True,
        help="the path to the subset of results to remove (in tsv format)",
    )
    subtract_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the new set of results (in tsv format)",
    )

    ###

    summarize_parser = subparsers.add_parser(
        "summarize",
        help="summarize a set of results",
    )
    summarize_parser.add_argument(
        "path",
        type=pathlib.Path,
        help="the path to the set of results (in tsv format)",
    )

    ###

    view_result_parser = subparsers.add_parser(
        "view-result",
        help="view one result of a set of results",
    )
    view_result_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the result",
    )
    view_result_parser.add_argument(
        "path",
        type=pathlib.Path,
        help="the path to the set of results (in tsv format)",
    )
    view_result_parser.add_argument(
        "line_number",
        type=int,
        help="the line number of the result to view",
    )

    # Setup

    csv.field_size_limit(sys.maxsize)

    # Routing

    args = parser.parse_args()

    if args.subcommand == "check-no-worse":
        check_no_worse_helper(
            before_path=args.previous_results,
            after_path=args.new_results,
        )
    elif args.subcommand == "deduplicate":
        remove_duplicates_helper(
            input_path=args.input,
            output_path=args.output,
        )
    elif args.subcommand == "download":
        refresh_binary()
        if args.language == "elm":
            run_many_helper(
                path=args.path,
                generator=db_iter.elm_json,
                runner=run_backend.elm_json,
                sample_limit=args.sample_limit,
                dry_run=True,
            )
        if args.language == "python":
            run_many_helper(
                path=args.path,
                generator=db_iter.python,
                runner=run_backend.python,
                sample_limit=args.sample_limit,
                dry_run=True,
            )
    elif args.subcommand == "filter":
        filter_helper(
            input_path=args.input,
            output_path=args.output,
            invert=args.invert,
            statuses=args.statuses,
        )
    elif args.subcommand == "gen-survey-code":
        refresh_binary()
        gen_survey_code_helper(
            input_path=args.input,
            output_path=args.output,
            language=args.language,
        )
    elif args.subcommand == "make-report":
        refresh_binary()
        make_report_helper(
            input_path=args.input,
            output_path=args.output,
            language=args.language,
        )
    elif args.subcommand == "refactor":
        refresh_binary()
        refactor_helper(
            language=args.language,
            show_input=args.prettify_input,
        )
    elif args.subcommand == "refactor-many":
        refresh_binary()
        rerun_many_helper(
            input_path=args.input,
            output_path=args.output,
            language=args.language,
            depth=args.depth,
            timing_breakdown=args.timing_breakdown,
            ablation=args.ablation,
        )
    elif args.subcommand == "subtract":
        subtract_helper(
            superset_path=args.superset,
            subset_path=args.subset,
            output_path=args.output,
        )
    elif args.subcommand == "summarize":
        summarize_helper(
            path=args.path,
        )
    elif args.subcommand == "view-result":
        refresh_binary()
        view_result_helper(
            path=args.path,
            line_number=args.line_number,
            show_code=prettify_elm_json if args.language == "elm" else show_python,
            show_synthed_code=prettify_elm if args.language == "elm" else show_python,
        )
    else:
        sys.exit(1)
