import ast
import json
import os
import subprocess

import datasets
import huggingface_hub

HF_TOKEN_KEY = "HF_TOKEN"

def login():
    if HF_TOKEN_KEY not in os.environ:
        raise ValueError(f"'{HF_TOKEN_KEY}' environment variable not defined")
    huggingface_hub.login(token=os.environ[HF_TOKEN_KEY])


def elm_json(sample_limit=None, start=0):
    """Iterates through The Stack database, yielding elm-format JSON
    representations of Elm function/variable definitions from Elm files.

    The Elm database has 90,637 samples."""

    login()

    ds = datasets.load_dataset(
        "bigcode/the-stack",
        data_dir="data/elm",
        streaming=True,
        split="train",
    )

    count = 0
    for sample in iter(ds):
        if count > sample_limit:
            break
        try:
            elm_format_output = subprocess.check_output(
                ["elm-format", "--stdin", "--json"],
                input=sample["content"].encode("utf-8"),
                stderr=subprocess.PIPE,
            )
            data_dict = json.loads(elm_format_output)
            body = data_dict["body"]
            for block in body:
                if block.get("tag") == "Definition":
                    yield sample["max_stars_repo_path"], block
        except subprocess.CalledProcessError as e:
            print("[elm-format error]", e.stderr)
        count += 1


def python(sample_limit=None):
    """Iterates through The Stack database, yielding Python AST objects from
    Jupyter Notebook cells.

    The Python database has between 1,450,000 and 1,460,000 samples."""

    login()

    ds = datasets.load_dataset(
        "bigcode/the-stack",
        data_dir="data/jupyter-notebook",
        streaming=True,
        split="train",
    )

    count = 0
    for sample in iter(ds):
        if count > sample_limit:
            break
        try:
            nb_dict = json.loads(sample["content"])
            for cell in nb_dict["cells"]:
                if cell["cell_type"] == "code":
                    code = None
                    if type(cell["source"]) == str:
                        code = cell["source"]
                    elif type(cell["source"]) == list:
                        code = "\n".join(cell["source"])
                    else:
                        continue
                    yield sample["max_stars_repo_path"], ast.parse(code)
        except Exception as e:
            print("[db_iter.python_cell exception]", e)
        count += 1
