# Getting Started Guide

To evaluate this artifact, we recommend using the included virtual machine with
[VirtualBox](https://www.virtualbox.org/) v7.0.14. Other versions will likely
work, but this is the version that we tested the virtual machine with.

Once opened in VirtualBox, take the following steps to "kick the tire" of this
artifact:

1. Sign into the user "cobbler" with password "cobbler" (no quotes)
2. Open the terminal via the dock on the left side of the screen
3. Run `cd ~/Desktop/cobbler`
4. Run `./cobbler`

If you see a welcome message, you should be good to go.

# Step-by-Step Instructions

_Total time estimate for evaluating this artifact:_ 1-2 hours using the
recommended strategies.

## Reproducing the examples in the paper

First, open up the provided virtual machine in VirtualBox and `cd` into the
root of the project repository (Steps 1-3 of the previous section).

Next, take a look at the examples in the `examples/` directory; these are all
the example inputs we describe in the paper (with a few typos fixed, such as
incorrect variable names, accidentally swapped `if`/`else` branches, etc.).

Our artifact, Cobbler, refactors programs like these to use library functions.
In the case of Elm programs, it refactors them to use functions like those found
in the Elm Standard Library (e.g. functional combinators like `map` and
`filter`). In the case of Python programs, it refactors them to use the NumPy
high-performance computing library.

**Action:** To run Cobbler on these examples, run the following commands from
the root of the repository (all commands in these instructions should be run
from the root of the repository unless otherwise stated):

- `cat examples/WithDefaultMap.elm | ./cobbler refactor --language=elm`
- `cat examples/FilterMapConcat.elm | ./cobbler refactor --language=elm`
- `cat examples/MapFilter.elm | ./cobbler refactor --language=elm`
- `cat examples/dot_product.py | ./cobbler refactor --language=python`
- `cat examples/rolling_sum.elm | ./cobbler refactor --language=python`

Note that, in Elm, `x |> f` is equivalent to `f x` (function application).

Also, in Python, we have a synthesis post-processing stage to ensure that the
arguments to functions like `np.multiply` are the same length (which NumPy
requires); this is why the fourth example above outputs the somewhat odd-looking
`y[:len(x)]` argument.

_Optional:_ If you're curious to try out the tool beyond reproducing the
examples in the paper, now would be a good time to do so!

## Reproducing the paper evaluation

### Step 1: Acquire input files

For our evaluation, we use input programs from
[The Stack](https://huggingface.co/datasets/bigcode/the-stack),
a dataset on
[Hugging Face](https://huggingface.co/).

**Action:** We provide two options to obtain these programs; please follow the
steps for exactly one of these. Option 1 is what we actually did to download the
programs originally. We obtained the data files in Option 2 by following the
exact steps from Option 1 ahead of time and caching the results. We recommend
Option 2 for the artifact evaluation.

- **Option 1** (**~4 hours** on ~50 Mbps internet): Create a
  [HuggingFace user access token](https://huggingface.co/docs/hub/security-tokens),
  set the environment variable `HF_TOKEN` to the value of this token (e.g.
  `export HF_TOKEN=<TOKEN STRING HERE>`), then run `bash
  evaluation/scripts/download_extract_real_world.sh`.
- **Option 2**: Copy the provided `<CACHE_DIR>/input/real_world_programs/`
  directory to `evaluation/input/real_world_programs/`. The `<CACHE_DIR>` on
  the virtual machine image is located at `~/Desktop/cache/`.

#### Checking that this step worked properly

Regardless of which option you chose above, the
`evaluation/input/real_world_programs/` directory should contain (at least) the
following two files after this step, which contain the test set input programs
described in Section 8.1 of the paper:

- `elm_test.tsv`
- `python_test.tsv`

### Step 2: Run evaluation

The evaluation, among other steps, include the following components that
**take a long time to run**:

- **Component 1:** Running Cobbler on all the programs with 10 replicates (**~80 minutes**)
- **Component 2:** NumPy performance evaluation with 10 replicates (**~14 hours**)

These times were taken on a 2020 MacBook Pro with a 2.3 GHz Quad-Core Intel Core
i7 processor and 32 GB of RAM (not on the virtual machine). It is likely that in
the virtual machine on your computer, these times might be even larger.

To speed up the evaluation, we also provide a "quick mode" evaluation that
**differs from the full evaluation (as described in the paper)** in the
following ways:

- 2 replicates are used for timing experiments rather than 10
- The maximum data size used for the NumPy performance evaluation is 10^6 rather
  than 10^8

From our testing, the results of the "quick mode" evaluation parallel the actual
results very closely. Moreover, "quick mode" evaluation reduces the running time
of **Component 1** above to **~25 minutes** and the running time of
**Component 2** above to **~2 minutes**.

**Action:** We provide two options for running the evaluation; please follow the
steps for exactly one of these. Option 1 is the full evaluation as described in
the paper, and Option 2 is the "quick mode" evaluation described above. We
recommend Option 2 for the artifact evaluation.

- **Option 1 (full evaluation):** Run the following command from the root of
  the repository:

        bash evaluation/run_everything_full.sh

- **Option 2 ("quick mode" evaluation; gives approximate results):** Run the
  following command from the root of the repository:

        bash evaluation/run_everything_approx.sh

Regardless of which method you choose to use, you will get a set of files that
can be used to validate the claims made in the paper. We describe how in the
next step.

#### Checking that this step worked properly

Regardless of which option you chose above, the `evaluation/output/data/`
directory should contain the following three files after this step:

- `elm_test.tsv`
- `python_test.tsv`
- `numpy_perf_eval.tsv`

The first two files contain the results of running Cobbler on the test set
from the previous step. The last file contains the results of the NumPy
performance evaluation.

We also provide the results of running this step on our machine ahead of time in
the provided `<CACHE_DIR>/output/data/` directory, which you can also copy into
the corresponding `evaluation/output/data/` directory, if desired (this will
overwrite the results generated in this step). The `<CACHE_DIR>` on the virtual
machine image is located at `~/Desktop/cache/`.

_Optional:_ If you're curious to see what the results from the first two files
above look like, you can run the following commands to generate human-readable
versions of these files:

- `./cobbler make-report --language=elm --input=evaluation/output/data/elm_test.tsv --output=elm_test_human_readable.elm`
- `./cobbler make-report --language=python --input=evaluation/output/data/python_test.tsv --output=python_human_readable.py`

### Step 3: Validate results

**Important note:** _Many (but not all) of the claims in the paper are about
performance results, for which it is extremely unlikely to reproduce exactly on
a virtual machine on different hardware. To validate these claims, we recommend
looking at rough trends to ensure that they align with the stated claims, rather
than for a precise match. Thus, **while we provide the exact steps needed to
validate the performance claims, we do not expect that these claims will be
exactly reproducible**._

The previous step also produces summaries/analyses/graphs of the data files in
the `evaluation/output/data/` directory described above. These analyses get
stored in the `evaluation/output/analyses` directory and can be used to validate
the claims in our evaluation.

Most of our claims are descriptive results of graphs in our evaluation.
Validating these claims amounts to validating that the graphs produced by the
previous steps are actually the graphs that we use in the paper. To validate
this, we provide a listing of files that contain the graphs we used for each
figure of our evaluation below.

**Action:** Please check that the following graphs match those in the paper.

- Fig. 4: `elm-astsize_dist.pdf` and `python-astsize_dist.pdf`
- Fig. 5: `synthtime_boxplot.pdf`
- Fig. 6: `elm-synthtime_vs_astsize.pdf` and `python-synthtime_vs_astsize.pdf`
- Fig. 7: `preferred.pdf`
- Fig. 8: `shrinkage-Prefer.pdf`
- Fig. 9: `speedup-penalized.pdf`

The remaining claims in our evaluation are about median timing information. We
provide a listing below of where to find evidence for claims about median timing
information that cannot be precisely determined by visual inspection of graphs
in the paper alone.

**Action:** Please validate the following claims using the files listed below,
taking into consideration the **Important note** at the start of this section.

- Fig. 5 - "The median synthesis time is <0.5s in all conditions": In
  `time-summary.txt`, all variables ending in C are <0.5.  *Note:* Variables
  ending in A are min times, B are 25th percentile times, C are 50th percentile
  (median) times, D are 75th percentile times, and E are max times.

- Section 8.2.2 - "The successful Elm and Python runs both took median synthesis
  times of 0.02s and the unsuccessful Elm and Python runs took median synthesis
  times of 0.21s and 0.39s respectively": This information can be found in the
  `time-summary.txt` file described above by looking at the corresponding
  variables ending in C.

- Fig. 9 - "By a data size of 10^8, this median [speedup] is 1.95× and 42/56
  programs are sped up >1×": This information is stored in the file
  `speedup_summary.txt`. The first number is found in the variable
  `PenalizedPerfSpeedupGMI`: "penalized speedup, performant NumPy functions,
  geometric median, data size 10^8" ("I" is the eighth letter of the alphabet).
  The second number is found in the variable `PenalizedPerfActualSpeedupCountI`:
  "penalized speedup, performant NumPy functions, actual speedup (i.e., speedup
  is >1×), data size 10^8."

And that's it! Thank you so much for your service as an artifact evaluator!

# Optional: Looking at the Cobbler codebase

If you would like to take a look at implementation of Cobbler, please refer to
the file `ARCHITECTURE.md` for how to dive in. You can also take a look at the
file `GITHUB_README.md`, which contains a README more typical of open-source
projects.
