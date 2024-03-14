# %% Import and configuration

import matplotlib.pyplot as plt

import numpy as np
import pandas as pd

import util

INPUT_DIR = "evaluation/output/data"
OUTPUT_DIR = "evaluation/output/analyses"

# %% Load data


# def summarize_stratified(filename):
#     df = pd.read_csv(f"{INPUT_DIR}/{filename}", sep="\t")
#     labels, counts = np.unique(
#         df["synthed ast size"].dropna().astype(int),
#         return_counts=True,
#     )
#     return list(zip(labels, counts))


def summarize(filename):
    df = pd.read_csv(f"{INPUT_DIR}/{filename}", sep="\t")
    return (df["status"] == "Success").sum()


elm = summarize("elm_test.tsv")
elm_ablation = summarize("elm_test_ablation.tsv")
python = summarize("python_test.tsv")
python_ablation = summarize("python_test_ablation.tsv")

python -= len(util.PYTHON_SUCCESS_ROWS_TO_DROP)
python_ablation -= len(util.PYTHON_SUCCESS_ROWS_TO_DROP)

# %% Plot data

# NUMBERS = "⓪①②③④⑤"
# def plot_stratified(data, data_ablation, title):
#     fig, ax = plt.subplots(1, 1, figsize=(2, 3))
#     assert len(data) == len(data_ablation)
#     bottom = [0, 0]
#     for (l, c), (al, ac) in zip(data, data_ablation):
#         assert l == al
#         section_label = NUMBERS[l]
#
#         b = ax.bar(
#             [0, 1],
#             [c, ac],
#             bottom=bottom,
#             width=0.7,
#             color="gray",
#             edgecolor="black",
#         )
#         ax.bar_label(
#             b,
#             labels=[section_label, section_label],
#             label_type="center",
#             fontsize=14,
#         )
#
#         bottom[0] += c
#         bottom[1] += ac
#
#     ax.bar_label(b, labels=bottom)
#
#     ax.set_xticks([0, 1], labels=["Full", "Ablated"])
#
#     max_count = max(bottom)
#     ystep = 50 if max_count < 300 else 500
#     ax.set_ylim(0, max_count * 1.1)
#     ax.set_yticks(np.arange(0, max_count, ystep))
#     ax.set_ylabel(r"$\bf{\#\ Successes}$")
#
#     ax.spines["top"].set_visible(False)
#     ax.spines["right"].set_visible(False)
#
#     ax.set_title(title, fontweight="bold", pad=0)
#
#     fig.tight_layout()
#     fig.savefig(f"{OUTPUT_DIR}/{title}-ablation.pdf")


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
