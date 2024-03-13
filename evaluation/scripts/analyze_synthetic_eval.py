# %% Import and configuration

import matplotlib.pyplot as plt

import numpy as np
import pandas as pd

INPUT_DIR = "evaluation/output/data"
OUTPUT_DIR = "evaluation/output/analyses"

# %% Load data

import numpy.random as r


def summarize(filename):
    df = pd.read_csv(f"{INPUT_DIR}/{filename}", sep="\t")[
        ["status", "synth time", "synthed ast size"]
    ]

    assert (df["status"] == "Success").all()
    for i, (_, row) in enumerate(df.iterrows()):
        assert row["synthed ast size"] == i + 1

    df["synth time"] = df["synth time"].apply(
        lambda row: [float(x) for x in row.split(",")]
    )

    df["synth time 25"] = df["synth time"].apply(lambda r: np.percentile(r, 25))
    df["synth time med"] = df["synth time"].apply(lambda r: np.median(r))
    df["synth time 75"] = df["synth time"].apply(lambda r: np.percentile(r, 75))

    return df[["synthed ast size", "synth time 25", "synth time med", "synth time 75"]]


elm = summarize("elm_synthetic.tsv")
python = summarize("python_synthetic.tsv")

# %% Plot data


def plot(data, title):
    fig, ax = plt.subplots(1, 1, figsize=(4, 3.5))

    x = data["synthed ast size"]
    y = np.log10(data["synth time med"])
    lo = np.log10(data["synth time 25"])
    hi = np.log10(data["synth time 75"])

    ax.errorbar(
        x,
        y,
        [y - lo, hi - y],
        marker="o",
        c="black",
    )

    ax.set_xticks(x)
    ax.set_yticks(np.arange(-2, 2.1, 0.5))

    ax.set_ylabel(r"log$_{10}$($\bf{Synthesis\ time}$ in seconds)")
    ax.set_xlabel(r"$\bf{\#\ Components}$ in solution")

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.set_title(title, fontweight="bold", pad=0)

    fig.tight_layout()
    fig.savefig(f"{OUTPUT_DIR}/{title}-synthetic.pdf")


plot(elm, "Elm")
plot(python, "Python")
