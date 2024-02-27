#!/usr/bin/env bash

# Download all data
./cobbler benchmark --language=elm --dry-run --sample-limit=90637 evaluation/data/elm-all-raw.tsv
./cobbler benchmark --language=python --dry-run --sample-limit=1000000 evaluation/data/python-all-raw.tsv

# Download training data
./cobbler benchmark --language=elm --dry-run --sample-limit=10000 evaluation/data/elm-train-raw.tsv
./cobbler benchmark --language=python --dry-run --sample-limit=100000 evaluation/data/python-train-raw.tsv

# Filter out any data that fails to pass extraction
./cobbler filter-benchmarks --input=evaluation/data/elm-all.tsv --output=evaluation/data/elm-all-filtered.tsv --invert ExtractFail
./cobbler filter-benchmarks --input=evaluation/data/python-all.tsv --output=evaluation/data/python-all-filtered.tsv --invert ExtractFail

# Run synthesizer on filtered data
./cobbler rerun-benchmarks --language=elm --input=evaluation/data/elm-all-filtered.tsv --output=evaluation/data/elm-all-synthduplicates.tsv
./cobbler rerun-benchmarks --language=python --input=evaluation/data/python-all-filtered.tsv --output=evaluation/data/python-all-synthduplicates.tsv

# Remove any duplicates
./cobbler remove-duplicates --input=evaluation/data/elm-all-synthduplicates.tsv --output=evaluation/data/elm-all-synth.tsv
./cobbler remove-duplicates --input=evaluation/data/python-all-synthduplicates.tsv --output=evaluation/data/python-all-synth.tsv

# Remove training data from all data results, leaving test data results
./cobbler subtract --superset=evaluation/data/elm-all-synth.tsv --subset=evaluation/data/elm-train-raw.tsv --output=evaluation/data/elm-test-synth.tsv
./cobbler subtract --superset=evaluation/data/python-all-synth.tsv --subset=evaluation/data/python-train-raw.tsv --output=evaluation/data/python-test-synth.tsv

# (For completeness) Remove test data from all data results, leaving training data results
./cobbler subtract --superset=evaluation/data/elm-all-synth.tsv --subset=evaluation/data/elm-test-synth.tsv --output=evaluation/data/elm-train-synth.tsv
./cobbler subtract --superset=evaluation/data/python-all-synth.tsv --subset=evaluation/data/python-test-synth.tsv --output=evaluation/data/python-train-synth.tsv

# Analyze test set results, including performance
cd test_set
python analyze_results.py
cd ..

# Run and analyze the Python/NumPy speedup evaluation
cd python_speedup
python run_performance_eval.py
python analyze_performance_eval.py
cd ..

# Analyze the Elm survey results
cd survey_analysis
python survey-analysis.py