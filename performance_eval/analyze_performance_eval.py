import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

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

# %% Compute summary


# Source: https://stackoverflow.com/a/43099751
def geo_mean(xs):
    return np.exp(np.log(xs).mean())


with open("performance_eval/output/speedup_summary.txt", "w") as f:
    f.write(
        "\\PerformanceEvalCount{" + str((~data["raw speedup 1"].isna()).sum()) + "}\n\n"
    )

    for p, alpha in zip(DATA_SIZE_POWERS, "ABCDEFG"):
        # Combined

        f.write(
            "\\RawSpeedupGM"
            + alpha
            + "{"
            + "{0:.2f}".format(geo_mean(data[f"raw speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\PenalizedSpeedupGM"
            + alpha
            + "{"
            + "{0:.2f}".format(geo_mean(data[f"penalized speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\RawActualSpeedupCount"
            + alpha
            + "{"
            + str((data[f"raw speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write(
            "\\PenalizedActualSpeedupCount"
            + alpha
            + "{"
            + str((data[f"penalized speedup {p}"] > 1).sum())
            + "}\n"
        )

        # Perf

        f.write(
            "\\RawPerfSpeedupGM"
            + alpha
            + "{"
            + "{0:.2f}".format(geo_mean(data[data["perf"] == 1][f"raw speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\PenalizedPerfSpeedupGM"
            + alpha
            + "{"
            + "{0:.2f}".format(
                geo_mean(data[data["perf"] == 1][f"penalized speedup {p}"])
            )
            + "}\n"
        )

        f.write(
            "\\RawPerfActualSpeedupCount"
            + alpha
            + "{"
            + str((data[data["perf"] == 1][f"raw speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write(
            "\\PenalizedPerfActualSpeedupCount"
            + alpha
            + "{"
            + str((data[data["perf"] == 1][f"penalized speedup {p}"] > 1).sum())
            + "}\n"
        )

        # NoPerf

        f.write(
            "\\RawNoPerfSpeedupGM"
            + alpha
            + "{"
            + "{0:.2f}".format(geo_mean(data[data["perf"] == 0][f"raw speedup {p}"]))
            + "}\n"
        )

        f.write(
            "\\PenalizedNoPerfSpeedupGM"
            + alpha
            + "{"
            + "{0:.2f}".format(
                geo_mean(data[data["perf"] == 0][f"penalized speedup {p}"])
            )
            + "}\n"
        )

        f.write(
            "\\RawNoPerfActualSpeedupCount"
            + alpha
            + "{"
            + str((data[data["perf"] == 0][f"raw speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write(
            "\\PenalizedNoPerfActualSpeedupCount"
            + alpha
            + "{"
            + str((data[data["perf"] == 0][f"penalized speedup {p}"] > 1).sum())
            + "}\n"
        )

        f.write("\n")

# %% Plot data


def make_plot(column_prefix):
    fig, ax = plt.subplots(1, 1, figsize=(12, 6))
    for _, row in data.iterrows():
        ax.plot(
            DATA_SIZE_POWERS,
            [np.log10(row[f"{column_prefix} speedup {p}"]) for p in DATA_SIZE_POWERS],
            marker="o",
            markersize=2,
            color="tab:blue" if row["perf"] == 1 else "tab:orange",
            alpha=0.2,
        )

    ax.set_xticks(
        DATA_SIZE_POWERS,
        # labels=[rf"$10^{p}$" for p in DATA_SIZE_POWERS],
    )

    ax.axhline(0, linewidth=1, color="black", linestyle="--")

    ax.set_xlabel(r"log$_{10}$($\bf{Data\ size}$)", fontsize=12)
    ax.set_ylabel(r"log$_{10}$($\bf{Speedup}$)", fontsize=12)

    fig.tight_layout()
    fig.savefig(f"performance_eval/output/speedup-{column_prefix}.pdf")


make_plot("raw")
make_plot("penalized")
