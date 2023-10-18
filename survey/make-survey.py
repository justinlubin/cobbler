#!/usr/bin/env python3

import os
import random
import subprocess

random.seed(100)

TEMPLATES_DIR = "templates"
QUESTIONS_PER_KIND = None  # None = all

MAX_SPLIT_SIZE = 25  # None = all (good size??: 100)

subprocess.run(["./clean-survey.sh"])

for kind in os.listdir("code"):
    if kind.startswith("."):
        continue

    rows = os.listdir(f"code/{kind}")
    rows = random.sample(rows, k=len(rows))

    split = 0
    count = 0
    for row in rows:
        if MAX_SPLIT_SIZE is not None and count == MAX_SPLIT_SIZE:
            split += 1
            count = 0

        if count == 0:
            subprocess.run(["mkdir", "-p", f"split-code/{kind}-{split}"])

        subprocess.run(
            [
                "cp",
                "-r",
                f"code/{kind}/{row}",
                f"split-code/{kind}-{split}/{row}",
            ]
        )

        count += 1

for kind in os.listdir("split-code"):
    if kind.startswith("."):
        continue

    subprocess.run(
        [
            "cp",
            f"{TEMPLATES_DIR}/survey-start.txt",
            f"survey/survey-{kind}.txt",
        ]
    )

    with open(f"survey/survey-{kind}.txt", "a") as survey_f:
        rows = os.listdir(f"split-code/{kind}")
        if QUESTIONS_PER_KIND:
            selected_rows = random.sample(rows, k=QUESTIONS_PER_KIND)
        else:
            selected_rows = random.sample(rows, k=len(rows))

        for i, row in enumerate(selected_rows):
            question_basename = f"q-{kind}-{i + 1}-"

            input_fname = f"split-code/{kind}/{row}/input.elm"
            output_fname = f"split-code/{kind}/{row}/output.elm"

            subprocess.run(["./code-to-html.sh", "elm", input_fname])
            subprocess.run(["./code-to-html.sh", "elm", output_fname])

            # Input then output
            survey_f.write(f"\n[[Block:{question_basename}io]]\n")
            survey_f.write(
                subprocess.check_output(
                    [
                        "cat",
                        f"{TEMPLATES_DIR}/question-start.txt",
                        f"{TEMPLATES_DIR}/option-a.txt",
                        f"{input_fname}.html",
                        f"{TEMPLATES_DIR}/option-b.txt",
                        f"{output_fname}.html",
                        f"{TEMPLATES_DIR}/question-end.txt",
                    ]
                ).decode("utf-8")
            )

            # Output then input
            survey_f.write(f"\n[[Block:{question_basename}oi]]\n")
            survey_f.write(
                subprocess.check_output(
                    [
                        "cat",
                        f"{TEMPLATES_DIR}/question-start.txt",
                        f"{TEMPLATES_DIR}/option-a.txt",
                        f"{output_fname}.html",
                        f"{TEMPLATES_DIR}/option-b.txt",
                        f"{input_fname}.html",
                        f"{TEMPLATES_DIR}/question-end.txt",
                    ]
                ).decode("utf-8")
            )

            subprocess.run(["rm", f"{input_fname}.html"])
            subprocess.run(["rm", f"{output_fname}.html"])
