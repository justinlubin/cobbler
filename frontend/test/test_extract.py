import os
from pathlib import Path
from lib.cbr_numpy.extractor import extract
import unittest
import sys
import ast
import json
sys.path.append("../..")

testdir = os.path.dirname(os.path.abspath(__file__))
fnames_dir = os.path.join(testdir, "test_data/programs/")
py_fnames = ["test4.py", "test5.py"]
json_fnames = ["../test4.json", "../test5.json"]


class TestExtractor(unittest.TestCase):

    def test_extractor(self):
        self.maxDiff = None
        for py_fname, json_fname in zip(py_fnames, json_fnames):
            with open(fnames_dir + py_fname, "r") as f:
                text = f.read()
                env, body = extract(text)
            with open(fnames_dir + json_fname, "r") as f:
                expected = json.load(f)
            self.assertEqual(ast.unparse(env), expected["env"])
            self.assertEqual(ast.unparse(body), expected["body"])


if __name__ == '__main__':
    unittest.main()
