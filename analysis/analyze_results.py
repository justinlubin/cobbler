# %% Import and configuration

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

np.random.seed(100)
# plt.rcParams["text.usetex"] = True

INPUT_DIR = "data/"
OUTPUT_DIR = "analysis/output/"

# %% Load data


def load_data(filename):
    df = pd.read_csv(INPUT_DIR + filename, sep="\t")[
        ["synthed ast size", "synth time", "synth time stddev", "status"]
    ]
    return df[df["status"].isin(["Success", "SynthFail"])]


data_elm = load_data("elm-final.tsv")
# data_python = load_data("python-final.tsv")

# %% Synthesis time vs. AST size


def synthtime_vs_astsize(data, name):
    fig, ax = plt.subplots(1, 1, figsize=(5, 5))
    ax.scatter(
        data["synthed ast size"] + 0.1 * np.random.rand(data.shape[0]) - 0.05,
        np.log10(data["synth time"]),
        marker=".",
        s=4,
    )

    ax.set_xticks(np.arange(1, data["synthed ast size"].max() + 1))
    ax.set_yticks([-2, -1, 0], labels=["0.01", "0.1", "1"])

    ax.set_xlabel("Output AST size (grammar expansions)")
    ax.set_ylabel(r"Synthesis time (seconds, log scale)")

    ax.set_title("Synthesis time vs. output AST size")

    fig.savefig(f"{OUTPUT_DIR}{name}/synthtime_vs_astsize.svg")


synthtime_vs_astsize(data_elm, "elm")

# %% Five-number summaries


def summarize(data, prefix):
    success = data[data["status"] == "Success"]["synth time"].describe()
    fail = data[data["status"] == "Success"]["synth time"].describe()

    print(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeA}{",
        success["min"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeB}{",
        success["25%"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeC}{",
        success["50%"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeD}{",
        success["75%"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeE}{",
        success["max"],
        "}",
        sep="",
    )

    print(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeA}{",
        fail["min"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeB}{",
        fail["25%"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeC}{",
        fail["50%"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeD}{",
        fail["75%"],
        "}",
        sep="",
    )
    print(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeE}{",
        fail["max"],
        "}",
        sep="",
    )


summarize(data_elm, "Elm")
