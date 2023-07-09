import subprocess
import sys

subprocess.run(
    "cobbler view-benchmark --language=python data/python-test-success.tsv "
    + sys.argv[1]
)

print("****************************************")

try:
    with open("performance_eval/programs/row" + sys.argv[1] + "/original.py", "r") as f:
        print(f.read())
except:
    print("<Missing original.py>")

print("****************************************")

try:
    with open(
        "performance_eval/programs/row" + sys.argv[1] + "/refactored.py", "r"
    ) as f:
        print(f.read())
except:
    print("<Missing refactored.py>")
