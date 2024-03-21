#!/usr/bin/env bash

INPUT_DIR="evaluation/output/data"
OUTPUT_DIR="evaluation/failures"

./cobbler filter --input="$INPUT_DIR/elm_test.tsv" --output="$OUTPUT_DIR/elm_test_failures.tsv" SynthFail
./cobbler filter --input="$INPUT_DIR/python_test.tsv" --output="$OUTPUT_DIR/python_test_failures.tsv" SynthFail

# 60 = 20 for Result, 20 for List, 3 (+17 overflow) for Maybe; should actually result in 43
./cobbler subsample --seed=0 --size=30 --input="$OUTPUT_DIR/elm_test_failures.tsv" --output="$OUTPUT_DIR/subsampled_elm_test_failures.tsv"
./cobbler subsample --seed=0 --size=20 --input="$OUTPUT_DIR/python_test_failures.tsv" --output="$OUTPUT_DIR/subsampled_python_test_failures.tsv"

./cobbler inputs --language=elm --input="$OUTPUT_DIR/subsampled_elm_test_failures.tsv" --output="$OUTPUT_DIR/samples/elm/"
./cobbler inputs --language=python --input="$OUTPUT_DIR/subsampled_python_test_failures.tsv" --output="$OUTPUT_DIR/samples/python/"
