# %% Import and configuration

import matplotlib.pyplot as plt

import numpy as np
import pandas as pd

INPUT_DIR = "evaluation/output/data"
OUTPUT_DIR = "evaluation/output/analyses"

# %% Load data

OCAML_TIME_FIELDS = [
    "ocaml_enumeration_time",
    "ocaml_canonicalization_time",
    "ocaml_unification_time",
]


def load_data(filename):
    df = pd.read_csv(f"{INPUT_DIR}/{filename}", sep="\t")[
        ["status"] + OCAML_TIME_FIELDS
    ]
    df = df[df["status"].isin(["Success", "SynthFail"])].reset_index(drop=True)

    # Enumeration times
    enum = df["ocaml_enumeration_time"].str.split(",", expand=True).astype(float)
    replicates = len(enum.columns)

    # Canonicalization times
    canon = df["ocaml_canonicalization_time"].str.split(",", expand=True).astype(float)
    assert len(canon.columns) == replicates

    # Unification times
    unif = df["ocaml_unification_time"].str.split(",", expand=True).astype(float)
    assert len(unif.columns) == replicates

    # Total (tracked) times
    total = enum + canon + unif

    # Percents

    df["enum % med"] = (enum / total).median(axis=1, skipna=True)
    df["canon % med"] = (canon / total).median(axis=1, skipna=True)
    df["unif % med"] = (unif / total).median(axis=1, skipna=True)

    df.drop(columns=OCAML_TIME_FIELDS, inplace=True)
    return df


data_elm = load_data("elm_test_timing_breakdown.tsv")
data_python = load_data("python_test_timing_breakdown.tsv")

# %% Plot data

BINS = np.arange(0, 1.05, 0.1)


def plot(df, title, subtitle=None):
    fig, ax = plt.subplots(
        3,
        1,
        figsize=(3, 2.5),
        layout="constrained",
    )
    fig.get_layout_engine().set(hspace=0.05)

    yticks = [0, 0.25, 0.5, 0.75, 1]
    yticklabels = ["0%", "", "50%", "", "100%"]

    for i, (cat, nice_cat) in enumerate(
        [
            ("canon", "Canonicalization"),
            ("unif", "Unification"),
            ("enum", "Enumeration"),
        ]
    ):
        vals = df[f"{cat} % med"]
        counts, _, _ = ax[i].hist(
            vals,
            bins=BINS,
            color="gray",
            edgecolor="black",
            weights=np.ones_like(vals) / len(vals),
        )

        median = vals.median()
        ax[i].axvline(x=median, c="#DD0000", lw=1.5)

        ax[i].set_xlim(-0.01, 1.01)
        ax[i].set_xticks(
            BINS,
            labels=[
                str(round(b * 100)) + "%" if i % 2 == 0 else ""
                for i, b in enumerate(BINS)
            ],
        )

        ax[i].set_ylim(0, 1)
        ax[i].set_yticks(yticks, labels=yticklabels)

        ax[i].spines["top"].set_visible(False)
        ax[i].spines["right"].set_visible(False)

        ax[i].text(
            0.5,
            0.5,
            nice_cat,
            ha="center",
            va="bottom",
            transform=ax[i].transAxes,
        )

    ax[2].set_xlabel(r"$\bf{Time\ taken}$", fontsize=10)
    ax[1].set_ylabel(r"$\bf Relative\ frequency$", fontsize=10)

    if subtitle:
        fig.suptitle(f"$\\bf {title}$ ({subtitle})")
        fig.savefig(f"{OUTPUT_DIR}/{title}-{subtitle}-timing_breakdown.pdf")
    else:
        fig.suptitle(f"$\\bf {title}$")
        fig.savefig(f"{OUTPUT_DIR}/{title}-timing_breakdown.pdf")


# plot(data_elm[data_elm["status"] == "Success"], "Elm", "Successful")
# plot(data_elm[data_elm["status"] == "SynthFail"], "Elm", "Unsuccessful")
#
# plot(data_python[data_python["status"] == "Success"], "Python", "Successful")
# plot(data_python[data_python["status"] == "SynthFail"], "Python", "Unsuccessful")

plot(data_elm, "Elm")
plot(data_python, "Python")
