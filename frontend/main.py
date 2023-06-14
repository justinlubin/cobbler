#!/usr/bin/env python3

import argparse
import csv
import pathlib
import sys

import db_iter
import benchmark


def benchmark_helper(path, generator=None, benchmarker=None, sample_limit=100):
    with open(path, "w") as f:
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

    args = parser.parse_args()

    if args.subcommand == "benchmark":
        if args.language == "elm":
            benchmark_helper(
                args.path_to_tsv,
                generator=db_iter.elm_json,
                benchmarker=benchmark.elm_json,
                sample_limit=1,
            )
        if args.language == "python":
            benchmark_helper(
                args.path_to_tsv,
                generator=db_iter.python_cell,
                benchmarker=benchmark.python_cell,
                sample_limit=1,
            )
