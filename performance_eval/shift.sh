#!/usr/bin/env bash

for i in $(seq $1 110);
do
  mv performance_eval/programs/row$i performance_eval/programs/row$(($i - 1))
done
