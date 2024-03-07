#!/usr/bin/env bash

INPUT_DIR="evaluation/input/real_world_programs"
OUTPUT_DIR="evaluation/output/data"

# ./cobbler refactor-many --language=elm --input="$INPUT_DIR/elm_train.tsv" --output="$OUTPUT_DIR/elm_train.tsv"
# ./cobbler refactor-many --language=python --input="$INPUT_DIR/python_train.tsv" --output="$OUTPUT_DIR/python_train.tsv"

./cobbler refactor-many --language=elm --input="$INPUT_DIR/elm_test.tsv" --output="$OUTPUT_DIR/elm_test.tsv"
./cobbler refactor-many --language=python --input="$INPUT_DIR/python_test.tsv" --output="$OUTPUT_DIR/python_test.tsv"
