import subprocess
import json
import datasets
import ast


def elm_json(sample_limit=None):
    """Iterates through The Stack database, yielding elm-format JSON
    representations of Elm function/variable definitions from Elm files"""
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
    Jupyter Notebook cells"""
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
