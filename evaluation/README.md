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
- `output/analyses`: All analyses of the result data in `output/data`
- `scripts`: The scripts to run and analyze the evaluation

    - **Important note:** These scripts should all be run from the _root
        directory_ of this repository!
    - *Tip:* To run these scripts in the proper order, run the
      `run_everything.sh` script from the root directory of this repository. The
      outputs will all be in the `output` directory.
