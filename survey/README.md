To generate the Qualtrics txt file:

1. In parent directory, run `./cobbler gen-survey-code --language=elm --input=data/elm-test-synth.tsv --output=survey/code`

2. In this directory, run `./make-survey.py`

The output is in `survey.txt`.
