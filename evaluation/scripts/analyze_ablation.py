# %% Import and configuration

import matplotlib.pyplot as plt

import numpy as np
import pandas as pd

import util

INPUT_DIR = "evaluation/output/data"
OUTPUT_DIR = "evaluation/output/analyses"

# %% Load data


def summarize(filename, drop):
    df = pd.read_csv(f"{INPUT_DIR}/{filename}", sep="\t")
    return len(
        df[df["status"] == "Success"]
        .reset_index()
        .drop(
            {r - 2 for r in drop},
            errors="ignore",
        )
    )


elm = summarize(
    "elm_test.tsv",
    drop=set(),
)
elm_ablation = summarize(
    "elm_test_ablation.tsv",
    drop=set(),
)
python = summarize(
    "python_test.tsv",
    drop=util.PYTHON_SUCCESS_ROWS_TO_DROP,
)
python_ablation = summarize(
    "python_test_ablation.tsv",
    drop=util.PYTHON_SUCCESS_ROWS_TO_DROP,
)

# %% Plot data


def plot(data, data_ablation, title):
    fig, ax = plt.subplots(1, 1, figsize=(2, 3))
    b = ax.bar(
        [0, 1],
        [data, data_ablation],
        width=0.7,
        color="gray",
        edgecolor="black",
    )
    ax.bar_label(b)
    ax.set_xticks([0, 1], labels=["Full", "Ablated"])

    max_count = max(data, data_ablation)
    ystep = 20 if max_count < 300 else 500
    ax.set_ylim(0, max_count * 1.1)
    ax.set_yticks(np.arange(0, max_count, ystep))
    ax.set_ylabel(r"$\bf{\#\ Successes}$")

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.set_title(title, fontweight="bold", pad=0)

    fig.tight_layout()
    fig.savefig(f"{OUTPUT_DIR}/{title}-ablation.pdf")


plot(elm, elm_ablation, "Elm")
plot(python, python_ablation, "Python")
