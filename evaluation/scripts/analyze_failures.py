# %% Import and configuration

import matplotlib
matplotlib.rcParams["pdf.fonttype"] = 42

import glob
import os

INPUT_DIR = "evaluation/failures/refactored"
OUTPUT_DIR = "evaluation/output/analyses"

LANGS = [("elm", "elm", "--"), ("python", "py", "#")]

# %% Compute reasons

# lang -> kind -> reason -> basenames
reasons: dict[str, dict[str | None, dict[str, list[str]]]] = {}

for lang, ext, comment_char in LANGS:
    reasons[lang] = {}

    reason_start = comment_char + " ***"

    for filename in sorted(glob.glob(f"{INPUT_DIR}/{lang}/*.{ext}")):
        kind = None
        if "-" in filename:
            kind = filename[filename.index("-") + 1 : filename.index(".")]

        basename = os.path.basename(filename)

        if kind not in reasons[lang]:
            reasons[lang][kind] = {}

        with open(filename, "r") as f:
            for line in f:
                if line.startswith(reason_start):
                    reason = line[len(reason_start) + 1 :].strip()
                    if reason not in reasons[lang][kind]:
                        reasons[lang][kind][reason] = []
                    reasons[lang][kind][reason].append(basename)

# %% Display reasons

with open(f"{OUTPUT_DIR}/failure_report.txt", "w") as f:
    for lang in reasons:
        f.write(lang.upper() + "\n")
        for kind in reasons[lang]:
            f.write(f"    Kind = {kind}\n")
            for reason in reasons[lang][kind]:
                f.write(f"        ({len(reasons[lang][kind][reason])}) {reason}: ")
                f.write(", ".join([f for f in reasons[lang][kind][reason]]) + "\n")
            f.write("\n")
