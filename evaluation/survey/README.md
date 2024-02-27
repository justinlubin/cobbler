To generate the Qualtrics txt file:

1. In this directory, run `./clean-code.sh`

1. In this directory, run `./clean-survey.sh`

2. In parent directory, run `./cobbler gen-survey-code --language=elm --input=INPUT_TSV.tsv --output=survey/code`

3. In this directory, run `./make-survey.py`

The output is in `survey.txt`.
