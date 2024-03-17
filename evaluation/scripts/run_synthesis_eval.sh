#!/usr/bin/env bash

INPUT_DIR="evaluation/input/real_world_programs"
OUTPUT_DIR="evaluation/output/data"

# echo "Standard, train (Elm)..."
# ./cobbler refactor-many --language=elm --input="$INPUT_DIR/elm_train.tsv" --output="$OUTPUT_DIR/elm_train.tsv"
# echo "Standard, train (Python)..."
# ./cobbler refactor-many --language=python --input="$INPUT_DIR/python_train.tsv" --output="$OUTPUT_DIR/python_train.tsv"

echo "Standard, test (Elm)..."
./cobbler refactor-many --language=elm --input="$INPUT_DIR/elm_test.tsv" --output="$OUTPUT_DIR/elm_test.tsv"
#echo "Standard, test (Python)..."
#./cobbler refactor-many --language=python --input="$INPUT_DIR/python_test.tsv" --output="$OUTPUT_DIR/python_test.tsv"

echo "Timing breakdown (Elm)..."
./cobbler refactor-many --timing-breakdown --language=elm --input="$INPUT_DIR/elm_test.tsv" --output="$OUTPUT_DIR/elm_test_timing_breakdown.tsv"
#echo "Timing breakdown (Python)..."
#./cobbler refactor-many --timing-breakdown --language=python --input="$INPUT_DIR/python_test.tsv" --output="$OUTPUT_DIR/python_test_timing_breakdown.tsv"

echo "Ablation (Elm)..."
./cobbler refactor-many --ablation --language=elm --input="$INPUT_DIR/elm_test.tsv" --output="$OUTPUT_DIR/elm_test_ablation.tsv"
#echo "Ablation (Python)..."
#./cobbler refactor-many --ablation --language=python --input="$INPUT_DIR/python_test.tsv" --output="$OUTPUT_DIR/python_test_ablation.tsv"
