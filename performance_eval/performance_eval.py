import csv
import glob
import os
import timeit


DATA_SIZE_POWERS = [1, 3, 5, 7]


def time_program(program, N):
    full_program = f"import numpy as np\nN = {N}\n{program}"
    try:
        return ",".join(
            [str(r) for r in timeit.repeat(stmt=full_program, repeat=10, number=1)]
        )
    except:
        return ""


def time_entry(subdir):
    with open(f"{subdir}/original.py", "r") as original_f:
        original_program = original_f.read()
    with open(f"{subdir}/refactored.py", "r") as refactored_f:
        refactored_program = refactored_f.read()
    return (
        {"program name": os.path.basename(subdir)}
        | {
            f"original {p}": time_program(original_program, 10**p)
            for p in DATA_SIZE_POWERS
        }
        | {
            f"refactored {p}": time_program(refactored_program, 10**p)
            for p in DATA_SIZE_POWERS
        }
    )


with open("performance_eval/output/performance_eval.tsv", "w", newline="") as f:
    writer = csv.DictWriter(
        f,
        fieldnames=["program name"]
        + [f"original {p}" for p in DATA_SIZE_POWERS]
        + [f"refactored {p}" for p in DATA_SIZE_POWERS],
        delimiter="\t",
    )

    writer.writeheader()

    for subdir in glob.glob("performance_eval/programs/*"):
        print(f"Benchmarking {subdir}...")
        writer.writerow(time_entry(subdir))

print("All done!")
