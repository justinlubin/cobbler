import unittest

import util
from python_to_ir import parse_str


fnames_dir = util.path_from_root("backend/test/test_cbr_numpy/test_data/programs/")
py_fnames = ["test1.py", "test2.py", "test3.py"]
sexp_fnames = ["test1.sexp", "test2.sexp", "test3.sexp"]


class TestParser(unittest.TestCase):
    def test_parser(self):
        self.maxDiff = None
        for py_fname, sexp_fname in zip(py_fnames, sexp_fnames):
            with open(fnames_dir + "/" + py_fname, "r") as f:
                text = f.read()
                sexp = parse_str(text)
            with open(fnames_dir + "/" + sexp_fname, "r") as f:
                text = f.read()
                text = (
                    text.replace("\n", "")
                    .replace("\t", "")
                    .replace("  ", "")
                    .replace("( (", "((")
                    .replace(")(", ") (")
                    .replace(") )", "))")
                )
            self.assertEqual(text, str(sexp))


if __name__ == "__main__":
    unittest.main()
