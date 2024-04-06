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
    return dict(
        zip(
            *np.unique(
                df[df["status"] == "Success"]
                .reset_index()
                .drop(
                    {r - 2 for r in drop},
                    errors="ignore",
                )["synthed ast size"]
                .astype(int),
                return_counts=True,
            )
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

NUMBERS = "⓪①②③④⑤"


def plot(data, data_ablation, title, *, sizes):
    fig, ax = plt.subplots(1, 1, figsize=(1.9, 2.2))

    data_total = sum(data.values())
    data_ablation_total = sum(data_ablation.values())

    if sizes:
        max_size = max(max(data.keys()), max(data_ablation.keys()))
        bottom = [0, 0]
        for size in range(1, max_size + 1):
            size_label = NUMBERS[size]
            b = ax.bar(
                [0, 1],
                [data[size], data_ablation[size]],
                bottom=bottom,
                width=0.7,
                color="gray",
                edgecolor="black",
            )
            ax.bar_label(
                b,
                labels=[size_label, size_label],
                label_type="center",
                fontsize=14,
            )
            bottom[0] += data[size]
            bottom[1] += data_ablation[size]
        ax.bar_label(b)
    else:
        b = ax.bar(
            [0, 1],
            [data_total, data_ablation_total],
            width=0.7,
            color="gray",
            edgecolor="black",
        )
        ax.bar_label(b, padding=2)

    ax.set_xticks([0, 1], labels=["Full", "Ablated"])

    max_count = max(data_total, data_ablation_total)
    ystep = 20 if max_count < 300 else 500
    ax.set_ylim(0, max_count * 1.1)
    ax.set_yticks(np.arange(0, max_count, ystep))
    ax.set_ylabel(r"$\bf{\#\ Successes}$")

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.set_title(title, fontweight="bold", pad=10)

    fig.tight_layout()
    fig.savefig(f"{OUTPUT_DIR}/{title}-ablation.pdf")


plot(elm, elm_ablation, "Elm", sizes=False)
plot(python, python_ablation, "Python", sizes=False)

with open(f"{OUTPUT_DIR}/ablation.txt", "w") as f:
    fmt = "{:,}"
    f.write(
        "\\newcommand{\\ElmAblation}{" + fmt.format(sum(elm_ablation.values())) + "}\n"
    )
    f.write(
        "\\newcommand{\\PythonAblation}{"
        + fmt.format(sum(python_ablation.values()))
        + "}\n"
    )
