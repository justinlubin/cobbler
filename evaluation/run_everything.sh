#!/usr/bin/env bash

SCRIPTS="evaluation/scripts"

# To run the "quick" evaluation, change the 0 in the following line to 1:
export COBBLER_QUICK_EVAL=0

# To use the cached real world programs, comment out this line and place the
# cached test set in `evaluation/input/real_world_programs`.
bash "$SCRIPTS/download_extract_real_world.sh"

bash "$SCRIPTS/run_synthesis_eval.sh"
python "$SCRIPTS/run_numpy_perf_eval.py"
python "$SCRIPTS/analyze_synthesis_eval.py"
python "$SCRIPTS/analyze_numpy_perf_eval.py"
python "$SCRIPTS/analyze_survey_results.py"
