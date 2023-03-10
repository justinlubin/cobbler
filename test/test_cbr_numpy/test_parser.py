import sys
sys.path.append("../..")
from lib.cbr_numpy.parser import parse

fname1 = "test_data/programs/test1.py"
fname2 = "test_data/programs/test2.py"

with open(fname2,"r") as f:
    text = f.read()
    print(parse(text))

