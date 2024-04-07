# %% Import

import matplotlib
matplotlib.rcParams["pdf.fonttype"] = 42

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import warnings

import util

warnings.simplefilter(
    action="ignore",
    category=pd.errors.PerformanceWarning,
)

# %% Config

INPUT_FILE = "evaluation/input/survey_results/survey_results.tsv"
OUTPUT_DIR = "evaluation/output/analyses"

STYLE_COLORS = {
    "Direct (input)": "#73D8F8",
    "Combinator (output)": "#BC89C5",
}

TYPE_COLORS = {
    "Maybe": "#73D8F8",
    "Result": "#BC89C5",
    "List": "#39B54A",
}

TYPE_MARKERS = {
    "Maybe": "o",
    "Result": "s",
    "List": "^",
}


# %% Load data files

raw_data = pd.read_csv(
    INPUT_FILE,
    sep="\t",
    encoding="utf-16",
    header=1,
)

# %% Clean data

cleaned_raw_data = (
    raw_data.drop(
        columns=raw_data.columns[raw_data.columns.str.startswith("FL_")],
    )
    .set_index("Response ID")
    .fillna("")
)

metadata = pd.DataFrame(index=cleaned_raw_data.index)
metadata["PL1"] = cleaned_raw_data.iloc[:, 10]
metadata["PL2"] = cleaned_raw_data.iloc[:, 11]
metadata["Exp"] = cleaned_raw_data.iloc[:, 12]

readdata = pd.DataFrame(index=cleaned_raw_data.index)
preferdata = pd.DataFrame(index=cleaned_raw_data.index)

comparison_columns = cleaned_raw_data.columns[
    cleaned_raw_data.columns.str.startswith("QID")
]

questions = {}

a = "Option (A)"
b = "Option (B)"


def clean_duplicates(row):
    r = row.values
    if r[0] == a:
        if r[1] == a:
            return ""
        elif r[1] == b:
            return "I"
        elif r[1] == "":
            return "I"
        else:
            assert False
    elif r[0] == b:
        if r[1] == a:
            return "O"
        elif r[1] == b:
            return ""
        elif r[1] == "":
            return "O"
        else:
            assert False
    elif r[0] == "":
        if r[1] == a:
            return "O"
        elif r[1] == b:
            return "I"
        elif r[1] == "":
            return ""
        else:
            assert False
    else:
        assert False


for q in range(0, len(comparison_columns), 6):
    qname, qval = comparison_columns[q].split(" - ", 1)
    qid = int(qname[3:])
    if 1915 <= qid and qid <= 2286:
        qtype = "List"
    elif 2287 <= qid and qid <= 2970:
        qtype = "Result"
    elif 2971 <= qid and qid <= 4168:
        qtype = "Maybe"
    else:
        assert False

    qname += "-" + qtype[0]

    questions[qname] = qval

    preferdata[qname] = (
        cleaned_raw_data[[comparison_columns[q], comparison_columns[q + 3]]]
    ).apply(clean_duplicates, axis=1)

    readdata[qname] = (
        cleaned_raw_data[[comparison_columns[q + 1], comparison_columns[q + 4]]]
    ).apply(clean_duplicates, axis=1)


# %% Make summary


def type_of_char(c):
    if c == "M":
        return "Maybe"
    elif c == "R":
        return "Result"
    elif c == "L":
        return "List"
    else:
        assert False


def cs_of_char(c):
    if c == "I":
        return "Direct (input)"
    elif c == "O":
        return "Combinator (output)"
    else:
        assert False


def summarize(data):
    summary = pd.DataFrame(
        index=[
            "M-I",
            "M-O",
            "R-I",
            "R-O",
            "L-I",
            "L-O",
        ],
    )

    summary["Type"] = summary.index.str[0].map(type_of_char)
    summary["Code Style"] = summary.index.str[-1].map(cs_of_char)
    summary["Count"] = 0
    summary["Total"] = 0

    for c in data.columns:
        qt = c[-1:]
        isum = (data[c] == "I").sum()
        osum = (data[c] == "O").sum()
        summary.loc[f"{qt}-I", "Count"] += isum
        summary.loc[f"{qt}-I", "Total"] += isum + osum
        summary.loc[f"{qt}-O", "Count"] += osum
        summary.loc[f"{qt}-O", "Total"] += isum + osum

    summary["Percent"] = summary["Count"] / summary["Total"]

    return summary


readsummary = summarize(readdata)
prefersummary = summarize(preferdata)

# %% Make sums


def make_sums(data):
    sums = pd.DataFrame(index=data.index)
    sums["I Sum"] = 0
    sums["O Sum"] = 0

    for idx, row in data.iterrows():
        sums.loc[idx, "I Sum"] += (row == "I").sum()
        sums.loc[idx, "O Sum"] += (row == "O").sum()

    sums["Percent-O"] = sums["O Sum"] / (sums["I Sum"] + sums["O Sum"])
    return sums


readsums = make_sums(readdata)
prefersums = make_sums(preferdata)


# %% Make bar plots


def simple_bars(summary, adjective, filename=None):
    fig, ax = plt.subplots(1, 1, figsize=(4, 4))

    util.bar_plot(
        ax,
        y=summary["Percent"],
        group_labels=summary["Type"],
        legend_labels=summary["Code Style"],
        legend_label_colors=STYLE_COLORS,
    )

    ax.set_ylim(0, 1)
    ax.set_yticks(np.arange(0, 1.1, 0.2))
    ax.yaxis.set_major_formatter(mtick.FuncFormatter("{:.0%}".format))
    # ax.set_ylabel("Total code marked as " + adjective)

    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.1), ncols=2)
    ax.set_title(f"Total code marked as {adjective}")
    fig.tight_layout()
    # fig.suptitle(f"Which style is {adjective}?")

    if filename:
        fig.savefig(f"{OUTPUT_DIR}/{filename}")


def stacked_bars(summary, adjective, filename=None):
    fig, ax = plt.subplots(1, 1, figsize=(3, 3.1))

    summary_i = summary[summary["Code Style"] == "Direct (input)"]
    summary_o = summary[summary["Code Style"] == "Combinator (output)"]

    o_bars = ax.bar(
        summary_o["Type"],
        summary_o["Percent"],
        label="Combinator (output)",
        color=STYLE_COLORS["Combinator (output)"],
        hatch="///",
        edgecolor="black",
    )
    ax.bar(
        summary_i["Type"],
        summary_i["Percent"],
        bottom=summary_o["Percent"],
        label="Direct (input)",
        color=STYLE_COLORS["Direct (input)"],
        edgecolor="black",
        clip_on=False,
    )
    ax.bar_label(
        o_bars,
        label_type="center",
        fmt="{:.0%}",
        c="white",
        fontweight="bold",
        bbox=dict(
            boxstyle="square",
            facecolor=STYLE_COLORS["Combinator (output)"],
            linewidth=0,
        ),
    )

    ax.set_xlim(-0.5, 2.5)
    ax.set_xticks(summary_i["Type"], labels=summary_i["Type"])

    ax.set_ylim(0, 1)
    ax.set_yticks(np.arange(0, 1.1, 0.2))
    ax.yaxis.set_major_formatter(mtick.FuncFormatter("{:.0%}".format))

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.legend(
        loc="upper center",
        bbox_to_anchor=(0.5, -0.15),
        ncols=1,
    )
    # ax.set_title(
    #     f"Total code marked as {adjective}",
    #     pad=10,
    # )
    fig.tight_layout()

    if filename:
        fig.savefig(f"{OUTPUT_DIR}/{filename}")


stacked_bars(readsummary, r"more $\bf{readable}$", filename="readable.pdf")
stacked_bars(prefersummary, r"$\bf{preferred}$", filename="preferred.pdf")

# %% Make experience scatter plots


def exp_scatter(exp_data, adjective, filename=None):
    fig, ax = plt.subplots(1, 1, figsize=(4, 3))
    ax.scatter(
        exp_data["Exp"],
        exp_data["Percent-O"],
        c="gray",
        edgecolors="black",
        linewidths=0.5,
        clip_on=False,
        zorder=10,
    )

    ax.set_xlim(0, exp_data["Exp"].max() + 1)
    ax.set_ylim(0, 1)
    ax.set_yticks(np.arange(0, 1.1, 0.2))
    ax.yaxis.set_major_formatter(mtick.FuncFormatter("{:.0%}".format))

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.set_ylabel("Combinator style " + adjective)
    ax.set_xlabel("Years experience with STFP")

    fig.tight_layout()

    if filename:
        fig.savefig(f"{OUTPUT_DIR}/{filename}")


read_exp_data = metadata.join(readsums)[["Exp", "Percent-O"]]
prefer_exp_data = metadata.join(prefersums)[["Exp", "Percent-O"]]

exp_scatter(
    read_exp_data,
    r"more $\bf{readable}$",
    filename="readable_exp.pdf",
)
exp_scatter(
    prefer_exp_data,
    r"$\bf{preferred}$",
    filename="prefer_exp.pdf",
)

# %% Question summary


def option_a(s):
    return s.split("Option (B)")[0][len("Option (A)") :].strip()


def option_b(s):
    return (
        s.split("Option (B)")[1]
        .split(
            "Which implementation of this function",
        )[0]
        .strip()
    )


qsum = pd.DataFrame(index=readdata.columns)
qsum["Read-I"] = readdata.sum().str.count("I")
qsum["Read-O"] = readdata.sum().str.count("O")
qsum["Prefer-I"] = preferdata.sum().str.count("I")
qsum["Prefer-O"] = preferdata.sum().str.count("O")

for q in qsum.index:
    qsum.loc[q, "IChars"] = len(option_a(questions[q]))
    qsum.loc[q, "OChars"] = len(option_b(questions[q]))

qsum["IChars"] = qsum["IChars"].astype(int)
qsum["OChars"] = qsum["OChars"].astype(int)

qsum["Shrinkage"] = qsum["IChars"] / qsum["OChars"]

# %% Plot summary


def shrinkage_plot(prefix, adjective):
    fig, ax = plt.subplots(1, 1, figsize=(5.5, 2.5))
    for ty in ["Maybe", "Result", "List"]:
        subdata = qsum[qsum.index.str.endswith(f"-{ty[0]}")]
        ax.scatter(
            np.log2(subdata["IChars"] / subdata["OChars"]),
            subdata[f"{prefix}-O"] / (subdata[f"{prefix}-I"] + subdata[f"{prefix}-O"]),
            label=ty,
            color=TYPE_COLORS[ty],
            marker=TYPE_MARKERS[ty],
            edgecolors="black",
            linewidths=0.5,
            # alpha=0.7,
            clip_on=False,
            zorder=10,
            s=15,
        )
    ax.axvline(0, linewidth=1, color="gray", linestyle="--")
    ax.set_xlabel(r"log$_2$($\bf{Direct}$ characters / $\bf{Combinator}$ characters)")
    ax.set_ylabel(r"% $\bf{Combinator}$ " + adjective)
    ax.set_ylim(0, 1)
    xlim = max(abs(ax.get_xlim()[0]), abs(ax.get_xlim()[1]))
    ax.set_xlim(-xlim, xlim)
    ax.set_yticks(np.arange(0, 1.1, 0.2))
    ax.yaxis.set_major_formatter(mtick.FuncFormatter("{:.0%}".format))
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.legend()
    fig.tight_layout()
    fig.savefig(f"{OUTPUT_DIR}/shrinkage-{prefix}.pdf")


shrinkage_plot("Read", "more readable")
shrinkage_plot("Prefer", "preferred")

# %% Shrinkage summary

# sns.displot(data=qsum, x="Shrinkage")

# %% Print survey info

with open(f"{OUTPUT_DIR}/survey_stats.txt", "w") as f:
    f.write("Number of participants: " + str(len(readsums)) + "\n")
    f.write(
        "Number of questions answered: "
        + str(readsums["I Sum"].sum() + readsums["O Sum"].sum())
        + "\n"
    )
