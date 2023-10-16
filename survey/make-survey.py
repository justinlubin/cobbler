#!/usr/bin/env python3

import os
import random
import subprocess

random.seed(100)

TEMPLATES_DIR = "templates"
QUESTIONS_PER_KIND = None  # None = all

subprocess.run(["cp", f"{TEMPLATES_DIR}/survey-start.txt", "survey.txt"])

with open("survey.txt", "w") as survey_f:
    for kind in os.listdir("code"):
        if kind.startswith("."):
            continue
        rows = os.listdir(f"code/{kind}")
        if QUESTIONS_PER_KIND:
            selected_rows = random.sample(rows, k=QUESTIONS_PER_KIND)
        else:
            selected_rows = random.sample(rows, k=len(rows))

        for i, row in enumerate(selected_rows):
            question_basename = f"q-{kind}-{i + 1}-"

            input_fname = f"code/{kind}/{row}/input.elm"
            output_fname = f"code/{kind}/{row}/output.elm"

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
