# %% Import and configuration

import matplotlib.pyplot as plt

import numpy as np
import pandas as pd

INPUT_DIR = "evaluation/output/data"
OUTPUT_DIR = "evaluation/output/analyses"

# %% Load data


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

    df["synth time min"] = df["synth time"].apply(lambda r: np.min(r))
    df["synth time med"] = df["synth time"].apply(lambda r: np.median(r))
    df["synth time max"] = df["synth time"].apply(lambda r: np.max(r))

    return df[
        ["synthed ast size", "synth time min", "synth time med", "synth time max"]
    ]


elm = summarize("elm_synthetic.tsv")
python = summarize("python_synthetic.tsv")

# %% Plot data

fig, ax = plt.subplots(1, 1, figsize=(2.7, 3))

DATAS = [elm, python]
NAMES = ["Elm", "Python"]
MARKERS = ["o", "s"]
COLORS = ["#73D8F8", "#BC89C5"]

for i, data in enumerate(DATAS):
    x = data["synthed ast size"]
    y = np.log10(data["synth time med"])
    lo = np.log10(data["synth time min"])
    hi = np.log10(data["synth time max"])

    ax.errorbar(
        x,
        y,
        [y - lo, hi - y],
        markeredgecolor="black",
        marker=MARKERS[i],
        c=COLORS[i],
        label=NAMES[i],
    )

ax.set_xticks(x)
ax.set_yticks(np.arange(-2, 2.1, 1.0))

ax.set_ylabel(r"log$_{10}$($\bf{Synthesis\ time}$ in seconds)")
ax.set_xlabel(r"$\bf{\#\ Components}$ in solution")

ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

ax.legend()

fig.tight_layout()
fig.savefig(f"{OUTPUT_DIR}/synthetic-scalability.pdf")
