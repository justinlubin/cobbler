./cobbler rerun-benchmarks --language=elm --input=data/oct10/oct10-train-success.tsv --output=data/oct10/oct10-train-success-again.tsv
cd survey
./clean-code.sh
cd ../
./cobbler gen-survey-code --language=elm --input=data/oct10/oct10-train-success-again.tsv --output=survey/code
cd survey
./make-survey.py
echo "Done!"
