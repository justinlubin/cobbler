# Evaluation Materials for "Refactoring Programs via Component-Based Synthesis Using Canonicalization-Based Equivalence Checking"

All section numbers here are in reference to the submitted PLDI 2024 paper
(#645).

This directory is organized as follows:

- `input`: The data that is used for the evaluation. This consists of:
    - `real_world_programs`: The real world programs from The Stack (see
      [The Stack README](README_TheStack.md) for important information about
      this data), as described in Section 8.1
    - `survey_results`: The results of the online survey surveying programmers'
      preferences, as described in Section 8.3
    - `manually_modified_numpy`: The manually-modified NumPy programs operating
      on increasing data sizes, as described in Section 8.4

- `output/data`: The tables of results of (i) running Cobbler on the `input`
  programs, as described in Section 8.1 and 8.2, and (ii) the NumPy performance
  evaluation, as described in Section 8.4


Files of note:

- Elm test set: `test_set/elm-test.tsv`
- Python test set: `test_set/python-test.tsv`
- Survey results: `survey_analysis/input/results.tsv`

Structure:

- `data`: the location for downloaded data from The Stack
- `python_speedup`: the data and analysis for the Python speedup evaluation
- `survey`: the files for constructing the Qualtrics survey
- `survey_analysis`: the results of the Qualtrics survey and the analysis scripts to analyze it
- `test_set`: the analysis of how well Cobbler does on the test set
