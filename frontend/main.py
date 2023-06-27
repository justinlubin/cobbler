#!/usr/bin/env python3

import argparse
import ast
import csv
import json
import pathlib
import subprocess
import sys

import benchmark
import db_iter
import util


def refresh_binary():
    try:
        subprocess.run(
            ["make", "regen-stdlib"],
            cwd=util.path_from_root("backend"),
            check=True,
        )
    except subprocess.CalledProcessError:
        sys.exit(1)

    try:
        subprocess.run(
            ["make", "build"],
            cwd=util.path_from_root("backend"),
            check=True,
        )
    except subprocess.CalledProcessError:
        sys.exit(1)


def show_elm_json(s):
    try:
        wrapped = '{"moduleName":"Main","imports":{},"body": [' + s + "]}"
        return "\n".join(
            subprocess.check_output(
                ["elm-format", "--stdin", "--from-json"],
                input=(wrapped).encode("utf8"),
                stderr=subprocess.PIPE,
            )
            .decode("utf8")
            .splitlines()[3:]
        )
    except subprocess.CalledProcessError as e:
        return "elm-format error: " + e.stderr.decode("utf8") + "\n\n" + wrapped


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


def show_python(s):
    return s


def refactor_helper(language=None):
    code = sys.stdin.read()

    if language == "elm":
        elm_format_output = subprocess.check_output(
            ["elm-format", "--stdin", "--json"],
            input=code.encode("utf-8"),
        )
        stats = benchmark.elm_json(json.loads(elm_format_output)["body"][0])
    elif language == "python":
        stats = benchmark.python(ast.parse(code))

    if stats["status"] == "Success":
        decoded_data = util.csv_str_decode(stats["synthed code"])
        if language == "elm":
            print(show_elm(decoded_data))
        elif language == "python":
            print(show_python(decoded_data))
    else:
        print("No solution found.")
        print("Status:", stats["status"])
        print("Reason:", stats["reason"] if "reason" in stats else "")
        sys.exit(1)


def benchmark_helper(
    path=None,
    generator=None,
    benchmarker=None,
    sample_limit=100,
    dry_run=None,
):
    with open(path, "w", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=benchmark.CSV_FIELDS,
            delimiter="\t",
        )
        writer.writeheader()
        previous_path = None
        sample_num = 0
        for sample_path, block in generator(sample_limit=sample_limit):
            if sample_path != previous_path:
                if previous_path is not None:
                    sample_num += 1
                    print(f"Completed '{previous_path}' ({sample_num}/{sample_limit})")
                previous_path = sample_path
            stats = benchmarker(block, dry_run=dry_run)
            writer.writerow(stats)
        print(f"Completed '{previous_path}' ({sample_num+1}/{sample_limit})")


def view_benchmark_helper(
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
                print(
                    "------------------------------------------------------------------------------------------------"
                )
                print(
                    "orig code:",
                    show_code(util.csv_str_decode(row["orig code"])),
                    sep="\n",
                )
                print(
                    "------------------------------------------------------------------------------------------------"
                )
                print(
                    "synthed code:",
                    show_synthed_code(util.csv_str_decode(row["synthed code"])),
                    sep="\n",
                )
                break


def make_report_helper(
    input_path=None,
    output_path=None,
    language=None,
):
    show_code = show_elm_json if language == "elm" else show_python
    show_synthed_code = show_elm if language == "elm" else show_python
    comment = "--" if language == "elm" else "##"
    with open(input_path, "r", newline="") as input_f:
        with open(output_path, "w") as output_f:
            for row in csv.DictReader(input_f, delimiter="\t"):
                output_f.write(
                    comment
                    + " =============================================================================\n"
                )
                output_f.write(comment + " Status: " + row["status"] + "\n")
                if row["reason"]:
                    output_f.write(comment + " Reason: " + row["reason"] + "\n")
                output_f.write(
                    comment + " Synthesis time: " + row["synth time"] + "\n\n"
                )
                output_f.write(comment + " Original code:\n\n")
                output_f.write(
                    show_code(util.csv_str_decode(row["orig code"])) + "\n\n"
                )
                if row["synthed code"]:
                    output_f.write(comment + " Synthesized code:\n\n")
                    output_f.write(
                        show_synthed_code(util.csv_str_decode(row["synthed code"]))
                        + "\n\n"
                    )


def filter_benchmarks_helper(
    input_path=None,
    output_path=None,
    invert=None,
    statuses=None,
):
    with open(input_path, "r", newline="") as input_f:
        with open(output_path, "w", newline="") as output_f:
            writer = csv.DictWriter(
                output_f,
                fieldnames=benchmark.CSV_FIELDS,
                delimiter="\t",
            )
            writer.writeheader()
            for row in csv.DictReader(input_f, delimiter="\t"):
                in_statuses = row["status"] in statuses
                if not invert and in_statuses or invert and not in_statuses:
                    writer.writerow(row)


def rerun_benchmarks_helper(
    input_path=None,
    output_path=None,
    language=None,
):
    if language == "elm":
        benchmarker = lambda s, **kwargs: benchmark.elm_json(json.loads(s))
    elif language == "python":
        benchmarker = lambda s, **kwargs: benchmark.python(ast.parse(s))

    def generator(sample_limit=None):
        with open(input_path, "r", newline="") as input_f:
            for i, row in enumerate(csv.DictReader(input_f, delimiter="\t")):
                yield f"Row {i + 2}", util.csv_str_decode(row["orig code"])

    benchmark_helper(
        path=output_path,
        generator=generator,
        benchmarker=benchmarker,
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
            row_failures.append(str(i + 1))

    if row_failures != []:
        print("[ERROR] The following rows got worse:", ", ".join(row_failures))
        sys.exit(1)
    else:
        print("All good!")


if __name__ == "__main__":
    if len(sys.argv) == 1:
        print(
            """   _________    ____  _   ______________
  / ____/   |  / __ \/ | / / ____/_  __/
 / / __/ /| | / /_/ /  |/ / __/   / /
/ /_/ / ___ |/ _, _/ /|  / /___  / /
\____/_/  |_/_/ |_/_/ |_/_____/ /_/"""
        )
        print("\nThe GARNET program synthesizer.\n")
        print(f"For help: {sys.argv[0]} --help")
        sys.exit(0)

    parser = argparse.ArgumentParser(description="The GARNET program synthesizer.")

    subparsers = parser.add_subparsers(
        title="subcommands",
        dest="subcommand",
        required=True,
    )

    # Refactor code subcommand

    refactor_parser = subparsers.add_parser(
        "refactor",
        help="refactor a snippet of code from stdin, printing to stdout",
    )
    refactor_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the code to refactor",
    )

    # Run benchmark suite subcommand

    benchmark_parser = subparsers.add_parser(
        "benchmark",
        help="run a benchmarking suite",
    )
    benchmark_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the synthesizer to benchmark",
    )
    benchmark_parser.add_argument(
        "--sample-limit",
        type=int,
        default=20,
        help="the maximum number of samples (files) to draw from the database (default: 20)",
    )
    benchmark_parser.add_argument(
        "path_to_tsv",
        type=pathlib.Path,
        help="the path to write the benchmarking tsv to",
    )
    benchmark_parser.add_argument(
        "--dry-run",
        action=argparse.BooleanOptionalAction,
        help="do not run the synthesizer on the benchmarks, only emit TSV",
    )

    # View benchmark result subcommand

    view_benchmark_parser = subparsers.add_parser(
        "view-benchmark",
        help="view a benchmark result",
    )
    view_benchmark_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the synthesizer to benchmark",
    )
    view_benchmark_parser.add_argument(
        "path_to_tsv",
        type=pathlib.Path,
        help="the path of the benchmarking tsv to view",
    )
    view_benchmark_parser.add_argument(
        "line_number",
        type=int,
        help="the line number of the benchmark entry to view",
    )

    # Make benchmark report subcommand

    make_report_parser = subparsers.add_parser(
        "make-report",
        help="make a nicely-formatted report from a benchmark result",
    )
    make_report_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the benchmark",
    )
    make_report_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path of the benchmarking tsv to make a report of",
    )
    make_report_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the report",
    )

    # Filter benchmark results subcommand

    filter_benchmarks_parser = subparsers.add_parser(
        "filter-benchmarks",
        help="view a benchmark result",
    )
    filter_benchmarks_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path of the benchmarking tsv to filter",
    )
    filter_benchmarks_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the new benchmarking tsv",
    )
    filter_benchmarks_parser.add_argument(
        "--invert",
        action=argparse.BooleanOptionalAction,
        help="exclude given statuses rather than include",
    )
    filter_benchmarks_parser.add_argument(
        "statuses",
        type=str,
        nargs="+",
        help="the statuses to filter",
    )

    # Re-run benchmark suite subcommand

    rerun_benchmark_parser = subparsers.add_parser(
        "rerun-benchmarks",
        help="run a benchmarking suite",
    )
    rerun_benchmark_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
        help="the language of the synthesizer to benchmark",
    )
    rerun_benchmark_parser.add_argument(
        "--input",
        type=pathlib.Path,
        required=True,
        help="the path of the benchmarking tsv to re-run",
    )
    rerun_benchmark_parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path to output the new benchmarking tsv",
    )

    # Summarize benchmark suite subcommand

    summarize_parser = subparsers.add_parser(
        "summarize",
        help="summarize a benchmarking suite",
    )
    summarize_parser.add_argument(
        "path_to_tsv",
        type=pathlib.Path,
        help="the path of the benchmarking tsv to summarize",
    )

    # Benchmark suite regression test subcommand

    check_no_worse_parser = subparsers.add_parser(
        "check-no-worse",
        help="ensure that a benchmark run is no worse than a previous one",
    )
    check_no_worse_parser.add_argument(
        "path_to_before_tsv",
        type=pathlib.Path,
        help='the path of the "before" benchmarking tsv',
    )
    check_no_worse_parser.add_argument(
        "path_to_after_tsv",
        type=pathlib.Path,
        help='the path of the "after" benchmarking tsv',
    )

    # Setup

    csv.field_size_limit(sys.maxsize)

    # Routing

    args = parser.parse_args()

    if args.subcommand == "refactor":
        refresh_binary()
        refactor_helper(
            language=args.language,
        )
    elif args.subcommand == "benchmark":
        refresh_binary()
        if args.language == "elm":
            benchmark_helper(
                path=args.path_to_tsv,
                generator=db_iter.elm_json,
                benchmarker=benchmark.elm_json,
                sample_limit=args.sample_limit,
                dry_run=args.dry_run,
            )
        if args.language == "python":
            benchmark_helper(
                path=args.path_to_tsv,
                generator=db_iter.python,
                benchmarker=benchmark.python,
                sample_limit=args.sample_limit,
                dry_run=args.dry_run,
            )
    elif args.subcommand == "view-benchmark":
        view_benchmark_helper(
            path=args.path_to_tsv,
            line_number=args.line_number,
            show_code=show_elm_json if args.language == "elm" else show_python,
            show_synthed_code=show_elm if args.language == "elm" else show_python,
        )
    elif args.subcommand == "make-report":
        make_report_helper(
            input_path=args.input, output_path=args.output, language=args.language
        )
    elif args.subcommand == "filter-benchmarks":
        filter_benchmarks_helper(
            input_path=args.input,
            output_path=args.output,
            invert=args.invert,
            statuses=args.statuses,
        )
    elif args.subcommand == "rerun-benchmarks":
        refresh_binary()
        rerun_benchmarks_helper(
            input_path=args.input,
            output_path=args.output,
            language=args.language,
        )
    elif args.subcommand == "summarize":
        summarize_helper(
            path=args.path_to_tsv,
        )
    elif args.subcommand == "check-no-worse":
        check_no_worse_helper(
            before_path=args.path_to_before_tsv,
            after_path=args.path_to_after_tsv,
        )
