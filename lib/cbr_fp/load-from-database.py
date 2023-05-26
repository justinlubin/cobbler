from huggingface_hub import login, hf_hub_download, try_to_load_from_cache

token="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"
login(token)
hf_hub_download(repo_id="bigcode/the-stack-dedup", filename="data/elm/data-00000-of-00001.parquet", repo_type="dataset")