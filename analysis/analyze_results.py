# %% Import and configuration

import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

import numpy as np
import pandas as pd

np.random.seed(100)

INPUT_DIR = "data/"
OUTPUT_DIR = "analysis/output/"

# %% Load data


def load_data(filename):
    df = pd.read_csv(INPUT_DIR + filename, sep="\t")[
        ["synthed ast size", "synth time", "synth time stddev", "status"]
    ]
    return df[df["status"].isin(["Success", "SynthFail"])]


data_elm = load_data("elm-final.tsv")
data_python = load_data("python-final.tsv")

# %% Synthesis time vs. AST size


def synthtime_vs_astsize(data, name):
    fig = plt.figure()

    gs = fig.add_gridspec(
        2,
        1,
        width_ratios=(1,),
        height_ratios=(1, 4),
        wspace=0.0,
        hspace=0.0,
    )

    ax = fig.add_subplot(gs[1, 0])

    spread = 0.3
    ax.scatter(
        data["synthed ast size"]
        + spread * np.random.rand(data.shape[0])
        - (spread / 2),
        np.log10(data["synth time"]),
        marker="o",
        s=5,
    )

    ax.set_xticks(np.arange(1, data["synthed ast size"].max() + 1))
    ax.set_yticks([-2, -1, 0], labels=["0.01", "0.1", "1"])

    ax.set_xlabel("# Components used")
    ax.set_ylabel("Synthesis time (s)")

    ax_hist = fig.add_subplot(gs[0, 0], sharex=ax)
    ax_hist.tick_params(axis="x", labelbottom=False)
    ax_hist.set_yticks([])
    ax_hist.set_frame_on(False)
    labels, counts = np.unique(
        data["synthed ast size"].dropna(),
        return_counts=True,
    )
    b = ax_hist.bar(
        labels,
        counts,
        align="center",
    )
    ax_hist.bar_label(b)

    fig.savefig(f"{OUTPUT_DIR}/{name}-synthtime_vs_astsize.pdf")


synthtime_vs_astsize(data_elm, "elm")
synthtime_vs_astsize(data_python, "python")

# %% Five-number summaries of synthesis time


def summarize(data, prefix, write):
    success = data[data["status"] == "Success"]["synth time"].describe()
    fail = data[data["status"] == "SynthFail"]["synth time"].describe()

    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeA}{",
        success["min"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeB}{",
        success["25%"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeC}{",
        success["50%"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeD}{",
        success["75%"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeE}{",
        success["max"],
        "}",
    )
    write()

    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeA}{",
        fail["min"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeB}{",
        fail["25%"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeC}{",
        fail["50%"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeD}{",
        fail["75%"],
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeE}{",
        fail["max"],
        "}",
    )
    write()


with open(f"{OUTPUT_DIR}time-summary.txt", "w") as f:

    def write(*args):
        f.write("".join(map(str, args)) + "\n")

    summarize(data_elm, "Elm", write)
    summarize(data_python, "Python", write)


# %% Box plots of synthesis time

fig, ax = plt.subplots(1, 1, figsize=(10, 4))
p = ax.boxplot(
    [
        np.log10(data_python["synth time"]),
        np.log10(data_elm["synth time"]),
    ],
    labels=["Python", "Elm"],
    whis=(0, 100),
    vert=False,
    # patch_artist=True,
)
# for median in p["medians"]:
#     median.set_color("black")
# for box in p["boxes"]:
#     box.set_facecolor("white")

ax.set_xticks([-2, -1, 0, 1], labels=["0.01", "0.1", "1", "10"])
ax.set_xlabel("Synthesis time (s)")

fig.savefig(f"{OUTPUT_DIR}synthtime_boxplot.pdf")
