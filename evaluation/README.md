# Evaluation Materials for "Refactoring Programs via Component-Based Synthesis Using Canonicalization-Based Equivalence Checking"

_All section and figure numbers here are in reference to the submitted PLDI 2024
paper (#645)._

This directory is organized as follows:

- `scripts`: The scripts to run and analyze the evaluation (these need to be run
  to fill in the rest of the directories).

    - **Important note:** These scripts should all be run from the _root
        directory_ of this repository!
    - _Tip:_ To run these scripts in the proper order, run the
      `run_everything.sh` script from the root directory of this repository. The
      outputs will all be in the `output` directory.

- `input`: The data that is used for the evaluation. This consists of:

    - `real_world_programs`: The real world programs from The Stack (see
      [The Stack README](README_TheStack.md) for important information about
      this data), as described in Section 8.1
        - To download and extract the data, use the
          `download_extract_real_world.sh` script
    - `survey_results`: The results of the online survey surveying programmers'
      preferences, as described in Section 8.3
    - `manually_modified_numpy`: The manually-modified NumPy programs operating
      on increasing data sizes, as described in Section 8.4

- `output/data`: The tables of results of (i) running Cobbler on the `input`
  programs, as described in Section 8.1 and 8.2, and (ii) the NumPy performance
  evaluation, as described in Section 8.4
    - To compute these, run the `run_*.sh` scripts

- `output/analyses`: All analyses/graphs of the results data from `output/data`
	- To compute these, run the `analyze_*` scripts
    - Fig. 4: `elm-astsize_dist.pdf` and `python-astsize_dist.pdf`
    - Fig. 5: `synthtime_boxplot.pdf`
    - Fig. 6: `elm-synthtime_vs_astsize.pdf` and `python-synthtime_vs_astsize.pdf`
    - Fig. 7: `preferred.pdf`
    - Fig. 8: `shrinkage-Prefer.pdf`
    - Fig. 9: `speedup-penalized.pdf`
