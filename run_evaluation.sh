#!/usr/bin/env bash

./cobbler benchmark --language=elm --dry-run --sample-limit=10000 data/elm-train.tsv
./cobbler benchmark --language=python --dry-run --sample-limit=100000 data/python-train.tsv

./cobbler benchmark --language=elm --dry-run --sample-limit=90637 data/elm-all.tsv
./cobbler benchmark --language=python --dry-run --sample-limit=1000000 data/python-all.tsv

./cobbler filter-benchmarks --input=data/elm-all.tsv --output=data/elm-filtered.tsv --invert ExtractFail
./cobbler filter-benchmarks --input=data/python-all.tsv --output=data/python-filtered.tsv --invert ExtractFail

./cobbler rerun-benchmarks --language=elm --input=data/elm-filtered.tsv --output=data/elm-pre-final.tsv
./cobbler rerun-benchmarks --language=python --input=data/python-filtered.tsv --output=data/python-pre-final.tsv

./cobbler remove-duplicates --input=data/elm-pre-final.tsv --output=data/elm-final.tsv
./cobbler remove-duplicates --input=data/python-pre-final.tsv --output=data/python-final.tsv

./cobbler subtract --superset=data/elm-final.tsv --subset=data/elm-train.tsv --output=data/elm-test.tsv
./cobbler subtract --superset=data/python-final.tsv --subset=data/python-train.tsv --output=data/python-test.tsv

python analysis/analyze_results.py
