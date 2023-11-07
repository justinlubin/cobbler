import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpt

# %% Load metadata

metadata = pd.read_csv("performance_eval/metadata.tsv", sep="\t")
metadata["synth time"] = metadata["synth time"].apply(
    lambda row: [float(x) for x in row.split(",")]
)
metadata["synth time med"] = metadata["synth time"].apply(lambda r: np.median(r))

metadata.index = np.char.add("row", (metadata.index.values + 2).astype(str))

# %% Load data

DATA_SIZE_POWERS = [0, 1, 2, 3, 4, 5, 6, 7, 8]

data = (
    pd.read_csv(
        "performance_eval/output/performance_eval.tsv",
        sep="\t",
        index_col="program name",
        keep_default_na=False,
    )
    .join(metadata)
    .sort_index()
)

for p in DATA_SIZE_POWERS:
    data[f"original {p}"] = data[f"original {p}"].apply(
        lambda row: [float(x) for x in row.split(",") if x != ""]
    )
    data[f"refactored {p}"] = data[f"refactored {p}"].apply(
        lambda row: [float(x) for x in row.split(",") if x != ""]
    )
    data[f"original {p} med"] = data[f"original {p}"].apply(lambda r: np.median(r))
    data[f"refactored {p} med"] = data[f"refactored {p}"].apply(lambda r: np.median(r))
    data[f"raw speedup {p}"] = data[f"original {p} med"] / data[f"refactored {p} med"]
    data[f"penalized speedup {p}"] = data[f"original {p} med"] / (
        data[f"refactored {p} med"] + data["synth time med"]
    )

data = data[~(data["raw speedup 1"].isna())]
assert len(data) == 102 - 2  # true successes - exponential examples


# %% Geometric mean/median helpers


def geo_mean(a, b):
    return np.sqrt(a * b)


def geo_median(xs):
    s = np.sort(xs[~np.isnan(xs)])
    n = len(s)
    if n % 2 == 1:
        return s[n // 2]
    else:
        return geo_mean(s[(n // 2) - 1], s[n // 2])


# %% Compute summary


with open("performance_eval/output/speedup_summary.txt", "w") as f:
    f.write(
        "\\newcommand{\\PerformanceEvalCount}{"
        + str((~(data["raw speedup 1"].isna())).sum())
        + "}\n"
    )
    f.write(
        "\\newcommand{\\PerformanceEvalCountPerf}{"
        + str((~(data[data["perf"] == 1]["raw speedup 1"].isna())).sum())
        + "}\n"
    )
    f.write(
        "\\newcommand{\\PerformanceEvalCountNoPerf}{"
        + str((~(data[data["perf"] == 0]["raw speedup 1"].isna())).sum())
        + "}\n\n"
    )

    fmt = "{0:.2f}"

    for p, alpha in zip(DATA_SIZE_POWERS, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"):
        # Combined

        f.write(
            "\\newcommand{\\RawSpeedupGM"
            + alpha
            + "}{"
            + fmt.format(geo_median(data[f"raw speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\newcommand{\\PenalizedSpeedupGM"
            + alpha
            + "}{"
            + fmt.format(geo_median(data[f"penalized speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\newcommand{\\RawActualSpeedupCount"
            + alpha
            + "}{"
            + str((data[f"raw speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write(
            "\\newcommand{\\PenalizedActualSpeedupCount"
            + alpha
            + "}{"
            + str((data[f"penalized speedup {p}"] > 1).sum())
            + "}\n"
        )

        # Perf

        f.write(
            "\\newcommand{\\RawPerfSpeedupGM"
            + alpha
            + "}{"
            + fmt.format(geo_median(data[data["perf"] == 1][f"raw speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\newcommand{\\PenalizedPerfSpeedupGM"
            + alpha
            + "}{"
            + fmt.format(geo_median(data[data["perf"] == 1][f"penalized speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\newcommand{\\RawPerfActualSpeedupCount"
            + alpha
            + "}{"
            + str((data[data["perf"] == 1][f"raw speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write(
            "\\newcommand{\\PenalizedPerfActualSpeedupCount"
            + alpha
            + "}{"
            + str((data[data["perf"] == 1][f"penalized speedup {p}"] > 1).sum())
            + "}\n"
        )

        # NoPerf

        f.write(
            "\\newcommand{\\RawNoPerfSpeedupGM"
            + alpha
            + "}{"
            + fmt.format(geo_median(data[data["perf"] == 0][f"raw speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\newcommand{\\PenalizedNoPerfSpeedupGM"
            + alpha
            + "}{"
            + fmt.format(geo_median(data[data["perf"] == 0][f"penalized speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\newcommand{\\RawNoPerfActualSpeedupCount"
            + alpha
            + "}{"
            + str((data[data["perf"] == 0][f"raw speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write(
            "\\newcommand{\\PenalizedNoPerfActualSpeedupCount"
            + alpha
            + "}{"
            + str((data[data["perf"] == 0][f"penalized speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write("\n")

# %% Plot data


def make_plot1(column_prefix):
    perf_color = "tab:blue"
    noperf_color = "tab:orange"

    fig, ax = plt.subplots(1, 1, figsize=(12, 6))
    for _, row in data.iterrows():
        ax.plot(
            DATA_SIZE_POWERS,
            [np.log10(row[f"{column_prefix} speedup {p}"]) for p in DATA_SIZE_POWERS],
            marker="o",
            markersize=2,
            color=perf_color if row["perf"] == 1 else noperf_color,
            alpha=0.05,
        )

    for perf in [0, 1]:
        for p in DATA_SIZE_POWERS:
            vals = data[data["perf"] == perf][f"{column_prefix} speedup {p}"]
            vals = vals[~(vals.isna())]
            log10vals = np.log10(vals)
            fg_parts = ax.violinplot(
                log10vals,
                positions=[p],
                vert=True,
                showmeans=False,
                showextrema=False,
                showmedians=False,
            )
            bg_parts = ax.violinplot(
                log10vals,
                positions=[p],
                vert=True,
                showmeans=False,
                showextrema=False,
                showmedians=False,
            )

            color = perf_color if perf == 1 else noperf_color

            for pc in fg_parts["bodies"]:
                pc.set_facecolor("None")
                pc.set_edgecolor(color)
                pc.set_alpha(1)

            for pc in bg_parts["bodies"]:
                pc.set_facecolor(color)
                pc.set_edgecolor("None")
                pc.set_alpha(0.2)

            ax.hlines(
                np.log10(geo_median(vals)),
                p - 0.2,
                p + 0.2,
                color=color,
            )

    ax.set_xticks(
        DATA_SIZE_POWERS,
        # labels=[rf"$10^{p}$" for p in DATA_SIZE_POWERS],
    )

    ax.axhline(0, linewidth=1, color="black", linestyle="--")

    ax.set_xlabel(r"log$_{10}$($\bf{Data\ size}$)", fontsize=12)
    ax.set_ylabel(r"log$_{10}$($\bf{Speedup}$)", fontsize=12)

    perf_patch = mpt.Patch(color=perf_color)
    noperf_patch = mpt.Patch(color=noperf_color)
    ax.legend(
        [perf_patch, noperf_patch],
        ["Uses performant NumPy functions", "Uses cosmetic functions only"],
        loc="lower right",
    )

    fig.tight_layout()
    fig.savefig(f"performance_eval/output/speedup-{column_prefix}.pdf")


def make_plot(column_prefix):
    perf_color = "#BC89C5"
    noperf_color = "#73D8F8"

    fig, ax = plt.subplots(1, 1, figsize=(8, 3.5))

    for perf in [0, 1]:
        for p in DATA_SIZE_POWERS:
            vals = data[data["perf"] == perf][f"{column_prefix} speedup {p}"]
            vals = vals[~(vals.isna())]
            log10vals = np.log10(vals)
            color = perf_color if perf == 1 else noperf_color
            ax.boxplot(
                log10vals,
                widths=0.3,
                positions=[p - 0.2 + (1 - perf) * 0.4],
                vert=True,
                flierprops={
                    "markersize": 1,
                    "marker": "o",
                    "markerfacecolor": None,
                    "markeredgecolor": "black",
                },
                boxprops={
                    "facecolor": color,
                },
                medianprops={
                    "color": "black",
                },
                # whiskerprops={
                #    "color": color,
                # },
                # capprops={
                #    "color": color,
                # },
                zorder=10,
                patch_artist=True,
            )
        # assert len(bp["boxes"]) == 1
        # bp["boxes"][0].set_facecolor(color)

    ax.set_xticks(
        DATA_SIZE_POWERS,
        labels=DATA_SIZE_POWERS,
    )

    ax.set_ylim(-6.5, 1.5)
    ax.set_yticks(np.arange(-6, 1.1, 1))

    ax.axhline(0, linewidth=1, color="gray", linestyle="--")

    ax.set_xlabel(r"log$_{10}$($\bf{Data\ size}$)", fontsize=12)
    ax.set_ylabel(r"log$_{10}$($\bf{Speedup}$)", fontsize=12)

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    # ax.spines["bottom"].set_visible(False)

    # ax.xaxis.set_ticks_position("none")

    perf_patch = mpt.Patch(color=perf_color)
    noperf_patch = mpt.Patch(color=noperf_color)
    ax.legend(
        [
            perf_patch,
            noperf_patch,
        ],
        [
            "Uses performant NumPy functions",
            "Uses cosmetic functions only",
        ],
        loc="lower right",
    )

    fig.tight_layout()
    fig.savefig(f"performance_eval/output/speedup-{column_prefix}.pdf")


make_plot("raw")
make_plot("penalized")
