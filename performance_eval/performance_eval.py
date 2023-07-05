import csv
import glob
import os
import timeit


def time(path):
    with open(path, "r") as f:
        program = f.read()
    return timeit.repeat(stmt=program, repeat=10, number=1)


def time_helper(subdir, title):
    return ",".join(map(str, time(f"{subdir}/{title}.py")))


filenames = [
    "original1",
    "original2",
    "original3",
    "original4",
    "refactored1",
    "refactored2",
    "refactored3",
    "refactored4",
]


with open("performance-eval/performance_eval.tsv", "w", newline="") as f:
    writer = csv.DictWriter(
        f,
        fieldnames=["program name"] + filenames,
        delimiter="\t",
    )

    writer.writeheader()

    for subdir in glob.glob("performance-eval/programs/*"):
        program_name = os.path.basename(subdir)
        print(f"Benchmarking {program_name}...")
        writer.writerow(
            {"program name": program_name}
            | {f: time_helper(subdir, f) for f in filenames},
        )

print("All done!")
