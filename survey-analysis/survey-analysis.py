# %% Import

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

# %% Load data files

raw_data = pd.read_csv(
    "results.tsv",
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


def simple_bars(summary, adjective):
    fig, ax = plt.subplots(1, 1, figsize=(3, 4))

    util.bar_plot(
        ax,
        y=summary["Percent"],
        group_labels=summary["Type"],
        legend_labels=summary["Code Style"],
        legend_label_colors={
            "Direct (input)": "#BC89C5",
            "Combinator (output)": "#73D8F8",
        },
    )

    ax.set_ylim(0, 1)
    ax.set_yticks(np.arange(0, 1.1, 0.2))
    ax.yaxis.set_major_formatter(mtick.FuncFormatter("{:.0%}".format))

    # fig.legend(bbox_to_anchor=(1.05, 1), loc="upper left")
    # fig.legend(loc="center left", bbox_to_anchor=(1, 0))
    fig.tight_layout()
    # fig.suptitle(f"Which style is {adjective}?")


simple_bars(readsummary, r"more $\bf{readable}$")
simple_bars(prefersummary, r"$\bf{preferred}$")

# %% Make experience scatter plots


def exp_scatter(exp_data):
    fig, ax = plt.subplots(1, 1, figsize=(4, 3))
    ax.scatter(exp_data["Exp"], exp_data["Percent-O"])

    ax.set_xlim(0, 100)
    ax.set_ylim(0, 1)
    ax.set_yticks(np.arange(0, 1.1, 0.2))
    ax.yaxis.set_major_formatter(mtick.FuncFormatter("{:.0%}".format))

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    ax.set_ylabel("Combinator style preferred")
    ax.set_xlabel("Years experience with STFP")

    fig.tight_layout()


read_exp_data = metadata.join(readsums)[["Exp", "Percent-O"]]
prefer_exp_data = metadata.join(prefersums)[["Exp", "Percent-O"]]

read_exp_data["Exp"] = np.random.random(2) * 100

exp_scatter(read_exp_data)
