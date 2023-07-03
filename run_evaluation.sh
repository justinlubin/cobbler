#!/usr/bin/env bash

./cobbler benchmark --language=elm --dry-run --sample-limit=10000 data/elm-all.tsv
./cobbler benchmark --language=python --dry-run --sample-limit=100000 data/python-all.tsv

./cobbler filter-benchmarks --input=data/elm-all.tsv --output=data/elm-filtered.tsv --invert ExtractFail
./cobbler filter-benchmarks --input=data/python-all.tsv --output=data/python-filtered.tsv --invert ExtractFail

./cobbler rerun-benchmarks --language=elm --input=data/elm-filtered.tsv --output=data/elm-final.tsv
./cobbler rerun-benchmarks --language=python --input=data/python-filtered.tsv --output=data/python-final.tsv
