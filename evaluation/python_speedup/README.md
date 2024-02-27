This directory contains the files related to the Python performance improvement
evaluation. As mentioned in the paper, we took the successfully-transformed
Python programs from the test set and manually modified them to accept varying sizes of data.

The successfully-transformed Python programs are in `python-test-success.tsv`.
Metadata about these programs is in `metadata.tsv`. The manually-translated
versions of these programs are in the `programs` directory.

To run the evaluation, run `run_performance_eval.py`, which will store its
outputs in the `output/` directory. To analyze these results (and produce
graphs), run `analyze_perfroamce_eval.py`, which will also store its outputs in
the `output/` directory.
