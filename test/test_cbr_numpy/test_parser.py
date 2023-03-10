import os
from pathlib import Path
from lib.cbr_numpy.parser import parse
import unittest
import sys
sys.path.append("../..")

testdir = os.path.dirname(os.path.abspath(__file__))
fnames_dir = os.path.join(testdir, "test_data/programs/")
py_fnames = ["test1.py", "test2.py", "test3.py"]
sexp_fnames = ["test1.sexp", "test2.sexp", "test3.sexp"]


class TestParser(unittest.TestCase):

    def test_parser(self):
        self.maxDiff = None
        for py_fname, sexp_fname in zip(py_fnames, sexp_fnames):
            with open(fnames_dir + py_fname, "r") as f:
                text = f.read()
                sexp = parse(text)
            with open(fnames_dir + sexp_fname, "r") as f:
                text = f.read()
                text = text.replace("\n", "").replace('\t', '').replace("  ", "").replace(
                    "( (", "((").replace(")(", ") (").replace(") )", "))")
            self.assertEqual(text, str(sexp))


if __name__ == '__main__':
    unittest.main()
