import unittest
import ast
import json

import extract
import util


fnames_dir = util.path_from_root("backend/test/test_cbr_numpy/test_data/programs/")
py_fnames = ["test4.py", "test5.py"]
json_fnames = ["../test4.json", "../test5.json"]


class TestExtractor(unittest.TestCase):
    def test_extractor(self):
        self.maxDiff = None
        for py_fname, json_fname in zip(py_fnames, json_fnames):
            with open(fnames_dir + "/" + py_fname, "r") as f:
                text = ast.parse(f.read())
                pre, body, _, _ = extract.python(text, rewrite_for=False)
            with open(fnames_dir + "/" + json_fname, "r") as f:
                expected = json.load(f)
            self.assertEqual(ast.unparse(pre), expected["env"])
            self.assertEqual(ast.unparse(body), expected["body"])


if __name__ == "__main__":
    unittest.main()
