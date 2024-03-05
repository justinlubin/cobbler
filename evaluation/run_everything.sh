#!/usr/bin/env bash

SCRIPTS="evaluation/scripts"

bash "$SCRIPTS/download_extract_real_world.sh"
bash "$SCRIPTS/run_synthesis_eval.sh"
python "$SCRIPTS/run_numpy_perf_eval.py"
python "$SCRIPTS/analyze_synthesis_eval.py"
python "$SCRIPTS/analyze_numpy_perf_eval.py"
python "$SCRIPTS/analyze_survey_results.py"
