import benchmarking
import csv
import json
import os
import sys
from  datasets  import  load_dataset
from huggingface_hub import login

TOKEN = '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'

def main():
    sys.path.append("../..")
    dir = os.path.dirname(os.path.abspath(__file__))
    output_csv = os.path.join(dir, "data/benchmarking/benchmarks.csv")

    # load Hugging Face dataset
    login(token=TOKEN)
    ds = load_dataset('bigcode/the-stack', data_dir='data/jupyter-notebook', streaming=True, split='train')

    benchmarking.build_benchmark()

    # write csv header
    with open(output_csv, 'w+', newline='') as file:
        file.truncate()
        writer = csv.DictWriter(file, fieldnames=benchmarking.CSV_FIELDS, extrasaction='ignore')
        writer.writeheader()

    # benchmark each notebook
    for sample in iter(ds):
        try:
            nb_dict = json.loads(sample['content'])
            if 'cells' in nb_dict:
                for cell in nb_dict['cells']:
                    if cell['cell_type'] == 'code':
                        stats = benchmarking.benchmark_cell(cell)
                        with open(output_csv, 'a', newline='') as file:
                            writer = csv.DictWriter(file, fieldnames=benchmarking.CSV_FIELDS, extrasaction='ignore')
                            writer.writerow(stats)
                print(f"finished benchmarking {sample['max_stars_repo_path']}")
                
        except Exception as e:
            print(e)


if __name__ == '__main__':
    main()