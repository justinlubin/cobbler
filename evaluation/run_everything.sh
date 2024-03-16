#!/usr/bin/env bash

SCRIPTS="evaluation/scripts"

bash "$SCRIPTS/run_synthesis_eval.sh"
#bash "$SCRIPTS/run_synthetic_eval.sh"
#python "$SCRIPTS/run_numpy_perf_eval.py"
#
#python "$SCRIPTS/analyze_synthesis_eval.py"
python "$SCRIPTS/analyze_timing_breakdown.py"
#python "$SCRIPTS/analyze_ablation.py"
#python "$SCRIPTS/analyze_synthetic_eval.py"
#python "$SCRIPTS/analyze_numpy_perf_eval.py"
#python "$SCRIPTS/analyze_survey_results.py"
