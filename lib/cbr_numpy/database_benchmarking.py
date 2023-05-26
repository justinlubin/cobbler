import benchmarking
import os
import sys
from  datasets  import  load_dataset
from huggingface_hub import login

TOKEN = "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"

def benchmark_db():
    login(token=TOKEN)
    ds = load_dataset("bigcode/the-stack", data_dir="data/jupyter-notebook", streaming=True, split="train")
    for sample in iter(ds):
        return

def main():
    benchmark_db()

if __name__ == '__main__':
    main()