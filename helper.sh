mkdir -p data/$1
./cobbler rerun-benchmarks --language=elm --input=data/elm-test-synth.tsv --output=data/$1/elm-test-synth.tsv
./cobbler filter-benchmarks --input=data/$1/elm-test-synth.tsv --output=data/$1/elm-test-synth-success.tsv Success
cd survey
./clean-code.sh
./clean-survey.sh
cd ../
./cobbler gen-survey-code --language=elm --input=data/$1/elm-test-synth-success.tsv --output=survey/code
cd survey
./make-survey.py
echo "Done!"
