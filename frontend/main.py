#!/usr/bin/env python3

import argparse
import csv
import pathlib
import subprocess
import sys

import benchmark
import db_iter
import util


def benchmark_helper(path=None, generator=None, benchmarker=None, sample_limit=100):
    try:
        subprocess.run(
            ["dune", "build", "bin/main.exe"],
            cwd=util.path_from_root("backend"),
            check=True,
        )
    except subprocess.CalledProcessError:
        sys.exit(1)

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
            stats = benchmarker(block)
            writer.writerow(stats)


def view_benchmark_helper(path=None, line_number=None, show_code=None):
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
                    util.csv_str_decode(row["synthed code"]),
                    sep="\n",
                )
                break


def show_elm(s):
    return "\n".join(
        subprocess.check_output(
            ["elm-format", "--stdin", "--from-json"],
            input=('{"moduleName":"Main","imports":{},"body": [' + s + "]}").encode("utf8"),
        )
        .decode("utf8")
        .splitlines()[3:]
    )


def show_python(s):
    return s


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

    # Run benchmark suite subcommand

    benchmark_parser = subparsers.add_parser(
        "benchmark",
        help="run a benchmarking suite",
    )
    benchmark_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
    )
    benchmark_parser.add_argument(
        "path_to_tsv",
        type=pathlib.Path,
        help="the path to write the benchmarking tsv to",
    )

    # View benchmark subcommand

    view_benchmark_parser = subparsers.add_parser(
        "view-benchmark",
        help="view a benchmark result",
    )
    view_benchmark_parser.add_argument(
        "--language",
        choices=["elm", "python"],
        required=True,
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

    # Routing

    args = parser.parse_args()

    if args.subcommand == "benchmark":
        if args.language == "elm":
            benchmark_helper(
                path=args.path_to_tsv,
                generator=db_iter.elm_json,
                benchmarker=benchmark.elm_json,
                sample_limit=20,
            )
        if args.language == "python":
            benchmark_helper(
                path=args.path_to_tsv,
                generator=db_iter.python,
                benchmarker=benchmark.python,
                sample_limit=20,
            )
    elif args.subcommand == "view-benchmark":
        view_benchmark_helper(
            path=args.path_to_tsv,
            line_number=args.line_number,
            show_code=show_elm if args.language == "elm" else show_python,
        )
