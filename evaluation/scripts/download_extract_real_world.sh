#!/usr/bin/env bash

BASE_DIR="evaluation/input/real_world_programs"

# Download all data
./cobbler benchmark --language=elm --dry-run --sample-limit=90637 "$BASE_DIR/elm_all_raw.tsv"
./cobbler benchmark --language=python --dry-run --sample-limit=1000000 "$BASE_DIR/python_all_raw.tsv"

# Download training data
./cobbler benchmark --language=elm --dry-run --sample-limit=10000 "$BASE_DIR/elm_train_raw.tsv"
./cobbler benchmark --language=python --dry-run --sample-limit=100000 "$BASE_DIR/python_train_raw.tsv"

# Filter out any data that fails to pass extraction (still has duplicates)
./cobbler filter-benchmarks --input="$BASE_DIR/elm_all_raw.tsv" --output="$BASE_DIR/elm_all_extracted.tsv" --invert ExtractFail
./cobbler filter-benchmarks --input="$BASE_DIR/python_all_raw.tsv" --output="$BASE_DIR/python_all_extracted.tsv" --invert ExtractFail

# Remove any duplicates
./cobbler remove-duplicates --input="$BASE_DIR/elm_all_extracted.tsv" --output="$BASE_DIR/elm_all.tsv"
./cobbler remove-duplicates --input="$BASE_DIR/python_all_extracted.tsv" --output="$BASE_DIR/python_all.tsv"

# Remove training input data from all input data, leaving test input data
./cobbler subtract --superset="$BASE_DIR/elm_all.tsv" --subset="$BASE_DIR/elm_train_raw.tsv" --output="$BASE_DIR/elm_test.tsv"
./cobbler subtract --superset="$BASE_DIR/python_all.tsv" --subset="$BASE_DIR/python_train_raw.tsv" --output="$BASE_DIR/python_test.tsv"

# Remove test input data from all input data, leaving training input data
./cobbler subtract --superset="$BASE_DIR/elm_all.tsv" --subset="$BASE_DIR/elm_test.tsv" --output="$BASE_DIR/elm_train.tsv"
./cobbler subtract --superset="$BASE_DIR/python_all.tsv" --subset="$BASE_DIR/python_test.tsv" --output="$BASE_DIR/python_train.tsv"