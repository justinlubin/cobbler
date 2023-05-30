import subprocess
import os
import csv
import sys
import json
from timeit import default_timer as timer


def benchmark(input_fname, out_filepath):
    all_stats = []
    # fields for csv
    fields = ['name',
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

    with open(input_fname, "r") as f:
        examples = f.read().split('\n')

    for i, ex in enumerate(examples):
        stats = {}
        dict = json.loads(ex)
        if dict["name"]:
            stats["name"] = dict["name"]
        else:
            stats["name"] = f"Benchmark #{i+1}"
        try:
            start = timer()
            output = subprocess.check_output(
                "_build/default/benchmark_fp/main.exe", input=ex, stderr=subprocess.PIPE, text=True)
            stats['synthed code'] = output
            end = timer()
            stats['synth time'] = end - start
        except subprocess.CalledProcessError as e:
            err = e.stderr
            if "Yojson" in err:
                stats["status"] = "ParseFail"
            else:
                stats["status"] = "SynthFail"
        all_stats.append(stats)

    # write to csv
    with open(out_filepath, 'r+', newline='') as file:
        writer = csv.DictWriter(file, fieldnames=fields, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(all_stats)


def main():
    sys.path.append("../..")
    dir = os.path.dirname(os.path.abspath(__file__))
    input_fname = os.path.join(dir, "examples.txt")
    output_csv = os.path.join(dir, "data/benchmarking/benchmarks.csv")

    benchmark(input_fname, output_csv)


if __name__ == '__main__':
    main()
