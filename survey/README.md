Dependencies to make survey images:

- pandoc
- phantomjs
- imagemagick

Run `./code-to-png.sh elm <filename>` to make a png out of code

In parent directory, run `./cobbler dump-successes --language=elm --input=data/elm-test.tsv --output=survey/code`

In this directory, run `./make-images.py`

Dependencies to make survey html:

- pandoc

In parent directory, run `./cobbler dump-successes --language=elm --input=data/elm-test.tsv --output=survey/code`

In this directory, run `./make-survey.py`
