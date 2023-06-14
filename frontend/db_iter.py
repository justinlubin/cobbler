import subprocess
import json
import datasets
import ast


def python_cell(sample_limit=None):
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
            elm_format_output_utf8, _ = subprocess.Popen(
                "elm-format --stdin --json",
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                shell=True,
            ).communicate(input=sample["content"].encode("utf-8"))
            elm_format_output = elm_format_output_utf8.decode("utf-8").strip()
            data_dict = json.loads(elm_format_output)
            body = data_dict["body"]
            for block in body:
                if block.get("tag") == "Definition":
                    yield sample["max_stars_repo_path"], block
        except Exception as e:
            print("[db_iter.elm_json exception]", e)
        count += 1
