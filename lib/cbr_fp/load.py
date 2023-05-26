from datasets import load_dataset
import sys
data = load_dataset("parquet", data_files=[r"\Users\kevin\.cache\huggingface\hub\datasets--bigcode--the-stack-dedup\snapshots\1d5c2bd9e6a7b7407332f0e004c2f4b22e7e9358\data\elm\data-00000-of-00001.parquet"], split="train[:20]")

def main():
    args = sys.argv[1:]
    print(data[int(args[0])]['content'])

if __name__ == "__main__":
    main()