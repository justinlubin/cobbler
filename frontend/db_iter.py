import subprocess
import json
import datasets


def python_cell(sample_limit=None):
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
                    yield sample["max_stars_repo_path"], cell
        except Exception as e:
            print(e)
        count += 1


def elm_json(sample_limit=None):
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
            print(e)
        count += 1
