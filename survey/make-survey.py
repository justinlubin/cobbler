#!/usr/bin/env python3

import os
import random
import subprocess

random.seed(100)

rows = os.listdir("code")

selected_rows = random.sample(rows, k=4)

subprocess.run(["cp", "templates/survey-start.txt", "survey.txt"])

with open("survey.txt", "a") as survey_f:
    for i, row in enumerate(selected_rows):
        q = f"q{i + 1:02d}"
        subprocess.run(["./code-to-html.sh", "elm", f"code/{row}/input.elm"])
        subprocess.run(["./code-to-html.sh", "elm", f"code/{row}/output.elm"])

        survey_f.write(f"\n[[Block:{q}-io]]\n")
        survey_f.write(
            subprocess.check_output(
                [
                    "cat",
                    # Input then output
                    "templates/question-start.txt",
                    "templates/option-a.txt",
                    f"code/{row}/input.elm.html",
                    "templates/option-b.txt",
                    f"code/{row}/output.elm.html",
                    "templates/question-end.txt",
                ]
            ).decode("utf-8")
        )

        survey_f.write(f"\n[[Block:{q}-oi]]\n")
        survey_f.write(
            subprocess.check_output(
                [
                    "cat",
                    "templates/question-start.txt",
                    "templates/option-a.txt",
                    f"code/{row}/output.elm.html",
                    "templates/option-b.txt",
                    f"code/{row}/input.elm.html",
                    "templates/question-end.txt",
                ]
            ).decode("utf-8")
        )
        subprocess.run(["rm", f"code/{row}/input.elm.html"])
        subprocess.run(["rm", f"code/{row}/output.elm.html"])
