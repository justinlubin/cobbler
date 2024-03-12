# %% Import and configuration

import matplotlib.pyplot as plt

import numpy as np
import pandas as pd

INPUT_DIR = "evaluation/output/data"
OUTPUT_DIR = "evaluation/output/analyses"

# %% Load data

OCAML_TIME_FIELDS = [
    "ocaml_synthesis_time",
    "ocaml_canonicalization_time",
    "ocaml_unification_time",
]


def load_data(filename):
    df = pd.read_csv(f"{INPUT_DIR}/{filename}", sep="\t")[
        ["status"] + OCAML_TIME_FIELDS
    ]
    df = df[df["status"].isin(["Success", "SynthFail"])].reset_index(drop=True)

    # Synthesis times
    sts = df["ocaml_synthesis_time"].str.split(",", expand=True).astype(float)
    replicates = len(sts.columns)

    # Canonicalization times and percents
    cts = df["ocaml_canonicalization_time"].str.split(",", expand=True).astype(float)
    assert len(cts.columns) == replicates
    cps = cts / sts
    df["canon % med"] = cps.median(axis=1)

    # Unification times and percents
    uts = df["ocaml_unification_time"].str.split(",", expand=True).astype(float)
    assert len(uts.columns) == replicates
    ups = uts / sts
    df["unif % med"] = ups.median(axis=1)

    # Enumeration times and percents
    ets = sts - (cts + uts)
    eps = ets / sts
    eps_med = eps.median(axis=1)
    df["enum % med"] = eps.median(axis=1)

    df.drop(columns=OCAML_TIME_FIELDS, inplace=True)
    return df


data_elm = load_data("elm_test.tsv")
data_python = load_data("python_test.tsv")

# %% Plot data

BINS = np.arange(0, 1.05, 0.1)


def plot(df, title, subtitle):
    fig, ax = plt.subplots(
        3,
        1,
        figsize=(2.5, 3),
        layout="constrained",
    )
    fig.get_layout_engine().set(hspace=0.05)

    yticks = [0, 0.33, 0.67, 1]
    yticklabels = [str(round(y * 100)) for y in yticks]

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

        ax[i].set_xlim(-0.00, 1.00)
        ax[i].set_xticks(
            BINS[::2],
            labels=[str(round(b * 100)) for b in BINS[::2]],
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

    ax[2].set_xlabel(r"$\bf{Time\ taken}$ (%)", fontsize=10)
    ax[1].set_ylabel(r"$\bf Relative\ frequency$ (%)", fontsize=10)

    fig.suptitle(f"$\\bf {title}$ ({subtitle})")
    fig.savefig(f"{OUTPUT_DIR}/{title}-{subtitle}-timing_breakdown.pdf")


plot(data_elm[data_elm["status"] == "Success"], "Elm", "Successful")
plot(data_elm[data_elm["status"] == "SynthFail"], "Elm", "Unsuccessful")

plot(data_elm[data_python["status"] == "Success"], "Python", "Successful")
plot(data_elm[data_python["status"] == "SynthFail"], "Python", "Unsuccessful")
