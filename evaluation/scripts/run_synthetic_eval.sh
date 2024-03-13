#!/usr/bin/env bash

INPUT_DIR="evaluation/input/synthetic_programs"
OUTPUT_DIR="evaluation/output/data"

python evaluation/scripts/generate_synthetic_programs.py

./cobbler make-tsv --language=elm --input=$INPUT_DIR --output="$INPUT_DIR/elm.tsv"
./cobbler make-tsv --language=python --input=$INPUT_DIR --output="$INPUT_DIR/python.tsv"

./cobbler refactor-many --depth=10 --language=elm --input="$INPUT_DIR/elm.tsv" --output="$OUTPUT_DIR/elm_synthetic.tsv"
./cobbler refactor-many --depth=10 --language=python --input="$INPUT_DIR/python.tsv" --output="$OUTPUT_DIR/python_synthetic.tsv"
