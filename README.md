# Getting Started Guide

To evaluate this artifact, we recommend using the included virtual machine with
[VirtualBox](https://www.virtualbox.org/) v7.0.14. Other versions will likely
work, but this is the version that we tested the virtual machine with.

Once opened in VirtualBox, take the following steps to "kick the tire" of this
artifact:

1. Sign into the only user
2. Open the terminal
3. Run `cd ~/Desktop/cobbler`
4. Run `./cobbler`

If you see a welcome message, you should be good to go.

# Step-by-Step Instructions

## Replicating the examples in the paper

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

To run Cobbler on these examples, run the following commands from the root of
the repository (all commands in these instructions should be run from the root
of the repository unless otherwise stated):

- `cat examples/WithDefaultMap.elm | ./cobbler refactor --language=elm`
- `cat examples/FilterMapConcat.elm | ./cobbler refactor --language=elm`
- `cat examples/MapFilter.elm | ./cobbler refactor --language=elm`
- `cat examples/dot_product.py | ./cobbler refactor --language=python`
- `cat examples/rolling_sum.elm | ./cobbler refactor --language=python`

Note that, in Elm, `x |> f` is equivalent to `f x` (function application).

_Optional:_ If you're curious to try out the tool beyond reproducing the
examples in the paper, now would be a good time to do so!

## Replicating the paper evaluation

### Step 1: Acquire input files

For our evaluation, we use input programs from
[The Stack](https://huggingface.co/datasets/bigcode/the-stack),
a dataset on
[HuggingFace](https://huggingface.co/).

We provide two options to obtain these programs; please follow the steps for
exactly one of these.

Option 1 is what we actually did to download the programs originally.

We obtained the data files in Option 2 by following the exact steps from
Option 1 ahead of time and caching the results.

- **Option 1** (**~4 hours** on ~50 Mbps internet): Create a
  [HuggingFace user access token](https://huggingface.co/docs/hub/security-tokens),
  set the environment variable `HF_TOKEN` to the value of this token (e.g.
  `export HF_TOKEN=<TOKEN STRING HERE>`), then run `bash
  evaluation/scripts/download_extract_real_world.sh`.
- **Option 2**: Copy the provided cached `cached/input/real_world_programs`
  directory to `evaluation/input/real_world_programs`.

#### Checking that this step worked properly

Regardless of which option you chose above, the
`evaluation/input/real_world_programs` directory should contain (at least) the
following two files after this step, which contain the test set input programs
described in Section 8.1 of the paper:

- `elm_test.tsv`
- `python_test.tsv`

### Step 2: Run evaluation

TODO

The evaluation, among other steps, include the following components that
**take a long time to run**:

- **Component 1:** Running Cobbler on all the programs with 10 replicates (**~80 minutes**)
- **Component 2:** NumPy performance evaluation with 10 replicates (**~14 hours**)

These times were taken on a 2020 MacBook Pro with a 2.3 GHz Quad-Core Intel Core
i7 processor and 32 GB of RAM. It is likely that in the virtual machine on your
computer, these times might be even larger.

To speed up the evaluation, we also provide a "quick mode" evaluation that
differs from the full evaluation (as described in the paper) in the following
ways:

- 2 replicates are used for timing experiments rather than 10
- The maximum data size used for the NumPy performance evaluation is 10^6 rather
  than 10^8

From our testing, the results of the "quick mode" evaluation parallel the actual
results very closely.

"Quick mode" evaluation reduces the running time of **Component 1** above to
~25 minutes and the running time of **Component 2** above to ~2 minutes.

To perform the "quick mode" replication, simply run the following command from 
the root of the repository:

For the sake of completeness, we will first describe how to fully replicate the
results in the paper in the section **Option 1: Full replication (slow)**.
However, among other steps, the evaluation includes the following components



As such, we describe some workarounds that we provide to combat these long
durations in the subsection **Option 2: Approximate replication (faster)**.

Please follow the steps for exactly one of these options.

### Option 1: Full replication (slow)

To replicate the paper evaluation, simply run the following command from the
root of the repository:

    bash evaluation/run_everything_full.sh

### Option 2: Approximate replication (faster)


    bash evaluation/run_everything_approx.sh

###

### "Quick mode" replication (much faster)

_Optional_: If you're curious to see what these programs look like, you can run
the following commands to generate human-readable versions of these files:
- `./cobbler make-report --language=elm --input=evaluation/input/real_world_programs/elm_test.tsv --output=elm_test_input.txt`
- `./cobbler make-report --language=python --input=evaluation/input/real_world_programs/python_test.tsv --output=python_test_input.txt`

Finally, _regardless of which method you choose to use_, you will get a set of
files that can be used to validate the claims made in the paper. We describe how
to do this in the section **Interpreting the results**.
