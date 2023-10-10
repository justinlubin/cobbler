This survey contains the code to make a Qualtrics survey txt file.

There is one additional dependency to make this txt file:

- pandoc

Steps:

1. In parent directory, run `./cobbler dump-successes --language=elm --input=data/elm-test-synth.tsv --output=survey/code`

2. In this directory, run `./make-survey.py`

The output is in `survey.txt`.
