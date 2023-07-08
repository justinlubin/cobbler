import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# %% Load data

DATA_SIZE_POWERS = [1, 3, 5]

data = pd.read_csv(
    "performance_eval/output/performance_eval.tsv",
    sep="\t",
    keep_default_na=False,
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
    data[f"raw speedup {p}"] = data[f"refactored {p} med"] / data[f"original {p} med"]
    data[f"penalized speedup {p}"] = (
        data[f"refactored {p} med"] / data[f"original {p} med"]
    )

# %% Compute summary


# Source: https://stackoverflow.com/a/43099751
def geo_mean(xs):
    return np.exp(np.log(xs).mean())


with open("performance_eval/output/speedup_summary.txt", "w") as f:
    for p, alpha in zip(DATA_SIZE_POWERS, "ABCDEFG"):
        raw_gm = geo_mean(data[f"raw speedup {p}"])
        penalized_gm = geo_mean(data[f"penalized speedup {p}"])
        f.write("\\RawSpeedup" + alpha + "{" + "{0:.2f}".format(raw_gm) + "}\n")
        f.write(
            "\\PenalizedSpeedup" + alpha + "{" + "{0:.2f}".format(penalized_gm) + "}\n"
        )

# %% Plot data


def make_plot(column_prefix):
    fig, ax = plt.subplots(1, 1, figsize=(12, 6))
    for _, row in data.iterrows():
        ax.plot(
            DATA_SIZE_POWERS,
            [np.log2(row[f"{column_prefix} speedup {p}"]) for p in DATA_SIZE_POWERS],
            marker="o",
            markersize=2,
            color="#1F77B4",
            alpha=0.2,
        )

    ax.set_xticks(
        DATA_SIZE_POWERS,
        # labels=[rf"$10^{p}$" for p in DATA_SIZE_POWERS],
    )

    ax.axhline(0, linewidth=1, color="black", linestyle="--")

    ax.set_xlabel(r"log$_{10}$($\bf{Data\ size}$)", fontsize=12)
    ax.set_ylabel(r"log$_2$($\bf{Speedup}$)", fontsize=12)

    fig.tight_layout()
    fig.savefig(f"performance_eval/output/speedup-{column_prefix}.pdf")


make_plot("raw")
make_plot("penalized")
