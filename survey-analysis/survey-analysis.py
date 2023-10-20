# %% Import

import numpy as np
import pandas as pd
import warnings

warnings.simplefilter(action="ignore", category=pd.errors.PerformanceWarning)

# %% Load data files

raw_data = pd.read_csv(
    "results.tsv",
    sep="\t",
    encoding="utf-16",
    header=1,
)

# %% Clean data

data = (
    raw_data.drop(
        columns=raw_data.columns[raw_data.columns.str.startswith("FL_")],
    )
    .set_index("Response ID")
    .fillna("")
)

metadata = pd.DataFrame(index=data.index)
metadata["PL1"] = data.iloc[:, 10]
metadata["PL2"] = data.iloc[:, 11]
metadata["Exp"] = data.iloc[:, 12]

readdata = pd.DataFrame(index=data.index)
preferdata = pd.DataFrame(index=data.index)


comparison_columns = data.columns[data.columns.str.startswith("QID")]

questions = {}

a = "Option (A)"
b = "Option (B)"


def correct(row):
    row = row.values
    if row[0] == a:
        if row[1] == a:
            return ""
        elif row[1] == b:
            return "I"
        elif row[1] == "":
            return "I"
        else:
            assert False
    elif row[0] == b:
        if row[1] == a:
            return "O"
        elif row[1] == b:
            return ""
        elif row[1] == "":
            return "O"
        else:
            assert False
    elif row[0] == "":
        if row[1] == a:
            return "O"
        elif row[1] == b:
            return "I"
        elif row[1] == "":
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
        data[[comparison_columns[q], comparison_columns[q + 3]]]
    ).apply(correct, axis=1)

    readdata[qname] = (
        data[[comparison_columns[q + 1], comparison_columns[q + 4]]]
    ).apply(correct, axis=1)

# %% Make summary

readsummary = pd.DataFrame(
    index=["M-I", "R-I", "L-I", "M-O", "R-O", "L-O"],
)

readsummary["Type"] = readsummary.index.str[0]
readsummary["IO"] = readsummary.index.str[-1]
readsummary["Count"] = 0
readsummary["Total"] = 0

prefersummary = pd.DataFrame(
    index=["M-I", "R-I", "L-I", "M-O", "R-O", "L-O"],
)

prefersummary["Type"] = prefersummary.index.str[0]
prefersummary["IO"] = prefersummary.index.str[-1]
prefersummary["Count"] = 0
prefersummary["Total"] = 0

for c in readdata.columns:
    qt = c[-1:]
    isum = (readdata[c] == "I").sum()
    osum = (readdata[c] == "O").sum()
    readsummary.loc[f"{qt}-I", "Count"] += isum
    readsummary.loc[f"{qt}-I", "Total"] += isum + osum
    readsummary.loc[f"{qt}-O", "Count"] += osum
    readsummary.loc[f"{qt}-O", "Total"] += isum + osum

for c in preferdata.columns:
    qt = c[-1:]
    isum = (preferdata[c] == "I").sum()
    osum = (preferdata[c] == "O").sum()
    prefersummary.loc[f"{qt}-I", "Count"] += isum
    prefersummary.loc[f"{qt}-I", "Total"] += isum + osum
    prefersummary.loc[f"{qt}-O", "Count"] += osum
    prefersummary.loc[f"{qt}-O", "Total"] += isum + osum

readsummary["Percent"] = readsummary["Count"] / readsummary["Total"]
prefersummary["Percent"] = prefersummary["Count"] / prefersummary["Total"]

# %% Make bar plots

g = sns.catplot(
    data=readsummary,
    x="Type",
    y="Percent",
    hue="IO",
    kind="bar",
)
