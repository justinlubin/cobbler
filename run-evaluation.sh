#!/usr/bin/env bash

# Download all data
./cobbler benchmark --language=elm --dry-run --sample-limit=90637 data/elm-all-raw.tsv
./cobbler benchmark --language=python --dry-run --sample-limit=1000000 data/python-all-raw.tsv

# Download training data
./cobbler benchmark --language=elm --dry-run --sample-limit=10000 data/elm-train-raw.tsv
./cobbler benchmark --language=python --dry-run --sample-limit=100000 data/python-train-raw.tsv

# Filter out any data that fails to pass extraction
./cobbler filter-benchmarks --input=data/elm-all.tsv --output=data/elm-all-filtered.tsv --invert ExtractFail
./cobbler filter-benchmarks --input=data/python-all.tsv --output=data/python-all-filtered.tsv --invert ExtractFail

# Run synthesizer on filtered data
./cobbler rerun-benchmarks --language=elm --input=data/elm-all-filtered.tsv --output=data/elm-all-synthduplicates.tsv
./cobbler rerun-benchmarks --language=python --input=data/python-all-filtered.tsv --output=data/python-all-synthduplicates.tsv

# Remove any duplicates
./cobbler remove-duplicates --input=data/elm-all-synthduplicates.tsv --output=data/elm-all-synth.tsv
./cobbler remove-duplicates --input=data/python-all-synthduplicates.tsv --output=data/python-all-synth.tsv

# Remove training data from all data results, leaving test data results
./cobbler subtract --superset=data/elm-all-synth.tsv --subset=data/elm-train-raw.tsv --output=data/elm-test-synth.tsv
./cobbler subtract --superset=data/python-all-synth.tsv --subset=data/python-train-raw.tsv --output=data/python-test-synth.tsv

# (For completeness) Remove test data from all data results, leaving training data results
./cobbler subtract --superset=data/elm-all-synth.tsv --subset=data/elm-test-synth.tsv --output=data/elm-train-synth.tsv
./cobbler subtract --superset=data/python-all-synth.tsv --subset=data/python-test-synth.tsv --output=data/python-train-synth.tsv

# Analyze results
python analysis/analyze_results.py

# Run and evaluate the NumPy performance evaluation
python performance_eval/run_performance_eval.py
python performance_eval/analyze_performance_eval.py
