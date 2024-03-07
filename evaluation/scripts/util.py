import os

import numpy as np
import matplotlib.pyplot as plt

def use_quick_eval():
    return (
        "COBBLER_QUICK_EVAL" in os.environ and
        os.environ["COBBLER_QUICK_EVAL"] == "1"
    )

DATA_SIZE_POWERS = [0, 1, 2, 3, 4, 5, 6] if use_quick_eval() else [0, 1, 2, 3, 4, 5, 6, 7, 8]


def bar_plot(
    ax,
    y=None,
    group_labels=None,
    legend_labels=None,
    legend_label_colors=None,
    width=0.8,
    na_zero_y_offset=0,
    group_spacing=1,
):
    """
    Plots a highly-configurable bar plot.

    Arguments:
        ax: The matplotlib axis to make the bar plot on

        y: The heights of the bars in the bar plot (one per bar)

        group_labels: a categorical label to assign each bar (one per bar);
            displayed on the x-axis

        legend_labels: a categorical label to assign each bar (one per bar);
            display in the legend

        legend_label_colors: the colors to assign each bar, based on its legend
            label; stored as a dictionary from legend labels to colors

        width: the width of the bar between 0 (exclusive) and 1 (inclusive)

        na_zero_y_offset: the y-position to plot the NA/0 textual indicator

        group_spacing: the amount of space (measured in bar widths) to separate
            each group

    """

    offset = 0
    xticks = []
    xticklabels = []
    group_start = None
    prev_group_label = None

    used_legend_labels = set()

    for yy, group_label, legend_label in zip(y, group_labels, legend_labels):
        if group_label != prev_group_label:
            if prev_group_label is not None:
                xticks.append((group_start + (offset - 1)) / 2)
                xticklabels.append(prev_group_label)
                offset += group_spacing

            group_start = offset

        color = legend_label_colors[legend_label]
        if legend_label in used_legend_labels:
            legend_label = None
        else:
            used_legend_labels.add(legend_label)

        ax.bar(
            offset,
            yy,
            width=width,
            color=color,
            label=legend_label,
        )

        if yy == 0:
            ax.text(
                offset,
                na_zero_y_offset,
                "0",
                ha="center",
                va="bottom",
                color=color,
                size=7,
            )

        if np.isnan(yy):
            ax.text(
                offset,
                na_zero_y_offset,
                "NA",
                ha="center",
                va="bottom",
                color=color,
                size=7,
            )

        offset += 1
        prev_group_label = group_label

    xticks.append((group_start + (offset - 1)) / 2)
    xticklabels.append(prev_group_label)

    ax.set_xlim(-1, offset)
    ax.set_xticks(xticks, labels=xticklabels)

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
