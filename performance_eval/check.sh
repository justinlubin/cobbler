#!/usr/bin/env bash

for i in $(seq 2 110);
do
  echo "Showing row $i!"
  echo
  tmp1=$(mktemp)
  tmp2=$(mktemp)
  ./cobbler view-benchmark --language=python data/python-test-success.tsv $i > $tmp1
  printf "\n\n\n\n\n" > $tmp2
  cat performance_eval/programs/row$i/original.py >> $tmp2
  printf "********************\n" >> $tmp2
  cat performance_eval/programs/row$i/refactored.py >> $tmp2
  printf "********************\n" >> $tmp2
  pr -w 160 -m -t $tmp1 $tmp2
  echo
  read -p "Press enter to continue"
done
