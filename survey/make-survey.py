#!/usr/bin/env python3

import os
import random
import subprocess

random.seed(100)

QUESTIONS_PER_KIND = 1

subprocess.run(["cp", "templates/survey-start.txt", "survey.txt"])

with open("survey.txt", "a") as survey_f:
    for kind in os.listdir("code"):
        if kind.startswith("."):
            continue
        rows = os.listdir(f"code/{kind}")
        selected_rows = random.sample(rows, k=QUESTIONS_PER_KIND)
        for i, row in enumerate(selected_rows):
            question_basename = f"q-{kind}-{i + 1}-"

            input_fname = f"code/{kind}/{row}/input.elm"
            output_fname = f"code/{kind}/{row}/output.elm"

            subprocess.run(["./code-to-html.sh", "elm", input_fname])
            subprocess.run(["./code-to-html.sh", "elm", output_fname])

            survey_f.write(f"\n[[Block:{question_basename}io]]\n")
            survey_f.write(
                subprocess.check_output(
                    [
                        "cat",
                        # Input then output
                        "templates/question-start.txt",
                        "templates/option-a.txt",
                        f"{input_fname}.html",
                        "templates/option-b.txt",
                        f"{output_fname}.html",
                        "templates/question-end.txt",
                    ]
                ).decode("utf-8")
            )

            # survey_f.write(f"\n[[Block:{question_basename}oi]]\n")
            # survey_f.write(
            #     subprocess.check_output(
            #         [
            #             "cat",
            #             "templates/question-start.txt",
            #             "templates/option-a.txt",
            #             f"{input_fname}.html",
            #             "templates/option-b.txt",
            #             f"{output_fname}.html",
            #             "templates/question-end.txt",
            #         ]
            #     ).decode("utf-8")
            # )

            subprocess.run(["rm", f"{input_fname}.html"])
            subprocess.run(["rm", f"{output_fname}.html"])
