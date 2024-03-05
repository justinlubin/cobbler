# %% Import and configuration

import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

import numpy as np
import pandas as pd

np.random.seed(100)

INPUT_DIR = "evaluation/output/data"
OUTPUT_DIR = "evaluation/output/analyses"

# %% Load data


def load_data(filename):
    df = pd.read_csv(f"{INPUT_DIR}/{filename}", sep="\t")[
        ["synthed ast size", "synth time", "status"]
    ]
    df = df[df["status"].isin(["Success", "SynthFail"])].reset_index(drop=True)
    df["synth time"] = df["synth time"].apply(
        lambda row: [float(x) for x in row.split(",")]
    )
    df["synth time med"] = df["synth time"].apply(lambda r: np.median(r))
    # Source: https://stackoverflow.com/a/23229224
    df["synth time iqr"] = df["synth time"].apply(
        lambda r: np.subtract(*np.percentile(r, [75, 25]))
    )
    df.drop(columns={"synth time"}, inplace=True)
    return df


data_elm = load_data("elm_test.tsv")
data_python = load_data("python_test.tsv")

# %% Handle Python bug

# These count as failures
PYTHON_SUCCESS_ROWS_TO_DROP = set([24, 29, 43, 44, 53, 87])

for row in PYTHON_SUCCESS_ROWS_TO_DROP:
    i = data_python[data_python["status"] == "Success"].index[row - 2]
    data_python.drop(labels=i, inplace=True)

# %% Component summary


def component_summarize(data, prefix, write):
    fmt = "{:,}"

    write(
        "\\newcommand{\\Num",
        prefix,
        "Interesting}{",
        fmt.format((data["synthed ast size"] > 1).sum()),
        "}",
    )

    write()


with open(f"{OUTPUT_DIR}/component-summary.txt", "w") as f:

    def write(*args):
        f.write("".join(map(str, args)) + "\n")

    component_summarize(
        data_elm,
        "Elm",
        write,
    )

    component_summarize(
        data_python,
        "Python",
        write,
    )

# %% Applicability summary


def applicability_summarize(data, prefix, write, dropped=None):
    assert dropped is not None

    num_success = (data["status"] == "Success").sum()
    num_fail = (data["status"] == "SynthFail").sum() + dropped
    num_total = num_success + num_fail
    assert num_total == len(data) + dropped

    fmt = "{:,}"

    write(
        "\\newcommand{\\Num",
        prefix,
        "Success}{",
        fmt.format(num_success),
        "}",
    )

    write(
        "\\newcommand{\\Num",
        prefix,
        "Fail}{",
        fmt.format(num_fail),
        "}",
    )

    write(
        "\\newcommand{\\Num",
        prefix,
        "SamplesTest}{",
        fmt.format(num_total),
        "}",
    )

    write(
        "\\newcommand{\\",
        prefix,
        "TestPercent}{",
        round(100 * (num_success / num_total)),
        "}",
    )

    write()


with open(f"{OUTPUT_DIR}/applicability-summary.txt", "w") as f:

    def write(*args):
        f.write("".join(map(str, args)) + "\n")

    applicability_summarize(
        data_elm,
        "Elm",
        write,
        dropped=0,
    )

    applicability_summarize(
        data_python,
        "Python",
        write,
        dropped=len(PYTHON_SUCCESS_ROWS_TO_DROP),
    )


# %% Synthesis time vs. AST size


def synthtime_vs_astsize_1(data, name):
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

    spread = 0.7
    ax.scatter(
        data["synthed ast size"]
        + spread * np.random.rand(data.shape[0])
        - (spread / 2),
        np.log10(data["synth time med"]),
        marker="o",
        s=4,
    )

    ax.set_xticks(np.arange(1, data["synthed ast size"].max() + 1))
    ax.set_yticks([-2, -1, 0])
    ax.set_ylim([-2, 0.5])

    ax.set_xlabel(r"$\bf{\#\ Components}$", fontsize=12)
    ax.set_ylabel(r"log$_{10}$($\bf{Synthesis\ time}$)", fontsize=12)

    ax_hist = fig.add_subplot(gs[0, 0], sharex=ax)
    ax_hist.tick_params(axis="x", labelbottom=False)
    ax_hist.set_yticks([])
    ax_hist.set_frame_on(False)
    labels, counts = np.unique(
        data["synthed ast size"].dropna(),
        return_counts=True,
    )
    b = ax_hist.bar(labels, counts, align="center", width=0.7)
    ax_hist.bar_label(b)
    ax_hist.set_ylabel(r"$\bf{\#\ Programs}$", fontsize=12)

    fig.align_ylabels()

    fig.tight_layout()
    fig.savefig(f"{OUTPUT_DIR}/{name}-synthtime_vs_astsize.pdf")


def synthtime_vs_astsize(data, name, title):
    fig, ax = plt.subplots(1, 1, figsize=(6, 2.5))

    sizes = []
    vals = []
    for size, subdata in data.groupby("synthed ast size"):
        sizes.append(size)
        vals.append(np.log10(subdata["synth time med"]).values)

    ax.boxplot(
        vals,
        labels=sizes,
        vert=False,
        # patch_artist=True,
        # boxprops={
        #     # "color": "#BC89C5",
        #     # "facecolor": "gray",
        # },
        # whiskerprops={
        #     "color": "#BC89C5",
        # },
        # capprops={
        #     "color": "#BC89C5",
        # },
        flierprops={
            "markersize": 1,
            "marker": "o",
            "markerfacecolor": "black",
            "markeredgecolor": None,
        },
        medianprops={
            "color": "black",
        },
    )

    ax.set_xticks(np.arange(-2, 0.1, 0.5))
    ax.set_xlim(-2.1, 0.1)
    ax.set_xlabel(r"log$_{10}$($\bf{Synthesis\ time}$ in seconds)", fontsize=12)

    ax.set_yticks(sizes, labels=[str(int(x)) for x in sizes])
    ax.set_ylabel(r"$\bf{\#\ Components}$", fontsize=12)

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.set_title(
        r"$\bf{" + title + r"}$",
        pad=12,
    )
    fig.tight_layout()
    fig.savefig(f"{OUTPUT_DIR}/{name}-synthtime_vs_astsize.pdf")


synthtime_vs_astsize(data_elm, "elm", "Elm")
synthtime_vs_astsize(data_python, "python", "Python")

# %% AST size histograms


def astsize_dist(data, name, title):
    fig, ax = plt.subplots(1, 1, figsize=(2.5, 3))

    labels, counts = np.unique(
        data["synthed ast size"].dropna(),
        return_counts=True,
    )

    b = ax.bar(
        labels,
        counts,
        align="center",
        width=0.7,
        color="gray",
        edgecolor="black",
    )
    ax.bar_label(b)

    ax.set_xticks(labels)
    ax.set_xlabel(r"$\bf{\#\ Components}$", fontsize=12)

    max_count = max(counts)
    ystep = 10 if max_count < 500 else 500
    ax.set_yticks(np.arange(0, max_count, ystep))
    ax.set_ylabel(r"$\bf{\#\ Programs}$", fontsize=12)

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.set_title(
        r"$\bf{" + title + r"}$",
        pad=12,
    )
    fig.tight_layout()
    fig.savefig(f"{OUTPUT_DIR}/{name}-astsize_dist.pdf")


astsize_dist(data_elm, "elm", "Elm")
astsize_dist(data_python, "python", "Python")


# %% Five-number summaries of synthesis time


def time_summarize(data, prefix, write):
    success = data[data["status"] == "Success"]["synth time med"].describe()
    fail = data[data["status"] == "SynthFail"]["synth time med"].describe()

    fmt = "{0:.2f}"

    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeA}{",
        fmt.format(success["min"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeB}{",
        fmt.format(success["25%"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeC}{",
        fmt.format(success["50%"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeD}{",
        fmt.format(success["75%"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "SuccessSynthTimeE}{",
        fmt.format(success["max"]),
        "}",
    )
    write()

    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeA}{",
        fmt.format(fail["min"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeB}{",
        fmt.format(fail["25%"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeC}{",
        fmt.format(fail["50%"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeD}{",
        fmt.format(fail["75%"]),
        "}",
    )
    write(
        "\\newcommand{\\",
        prefix,
        "FailSynthTimeE}{",
        fmt.format(fail["max"]),
        "}",
    )
    write()


def synthesis_deviation(df, write):
    iqr_factor = (df["synth time iqr"] / df["synth time med"]).max()

    write(
        "\\newcommand{\\SynthTimeIQRPercent}{",
        "{0:.2f}".format(100 * iqr_factor),
        "}",
    )
    write()


with open(f"{OUTPUT_DIR}/time-summary.txt", "w") as f:

    def write(*args):
        f.write("".join(map(str, args)) + "\n")

    time_summarize(data_elm, "Elm", write)
    time_summarize(data_python, "Python", write)

    synthesis_deviation(pd.concat([data_elm, data_python]), write)


# %% Box plots of synthesis time

# fig, ax = plt.subplots(1, 1, figsize=(10, 2.4))
# p = ax.boxplot(
#     [
#         np.log10(data_python[data_python["status"] == "SynthFail"]["synth time med"]),
#         np.log10(data_elm[data_elm["status"] == "SynthFail"]["synth time med"]),
#         np.log10(data_python[data_python["status"] == "Success"]["synth time med"]),
#         np.log10(data_elm[data_elm["status"] == "Success"]["synth time med"]),
#     ],
#     labels=[
#         r"$\bf{Python}$ (unsuccessful)",
#         r"$\bf{Elm}$ (unsuccessful)",
#         r"$\bf{Python}$ (successful)",
#         r"$\bf{Elm}$ (successful)",
#     ],
#     whis=(0, 100),
#     vert=False,
#     # showmedians=True,
#     # showmeans=False,
#     # showextrema=True,
# )
#
# ax.set_xticks([-2, -1, 0, 1])
# ax.set_xlabel(r"log$_{10}$($\bf{Synthesis\ time}$)", fontsize=12)
#
# fig.tight_layout()
# fig.savefig(f"{OUTPUT_DIR}/synthtime_boxplot.pdf")

fig, ax = plt.subplots(1, 1, figsize=(7.5, 2.5))
p = ax.boxplot(
    [
        np.log10(data_python[data_python["status"] == "SynthFail"]["synth time med"]),
        np.log10(data_elm[data_elm["status"] == "SynthFail"]["synth time med"]),
        np.log10(data_python[data_python["status"] == "Success"]["synth time med"]),
        np.log10(data_elm[data_elm["status"] == "Success"]["synth time med"]),
    ],
    labels=[
        r"$\bf{Python}$ (unsuccessful)",
        r"$\bf{Elm}$ (unsuccessful)",
        r"$\bf{Python}$ (successful)",
        r"$\bf{Elm}$ (successful)",
    ],
    vert=False,
    # showmedians=True,
    # showmeans=False,
    # showextrema=True,
    flierprops={
        "markersize": 1,
        "marker": "o",
        "markerfacecolor": "black",
        "markeredgecolor": None,
    },
    medianprops={
        "color": "black",
    },
)

ax.set_xticks(np.arange(-2, 1.1, 0.5))
ax.set_xlabel(r"log$_{10}$($\bf{Synthesis\ time}$ in seconds)", fontsize=12)

ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

fig.tight_layout()
fig.savefig(f"{OUTPUT_DIR}/synthtime_boxplot.pdf")
