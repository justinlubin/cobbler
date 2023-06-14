#!/usr/bin/env bash

for f in $(ls src); do
  elm-format --json src/$f | jq > json/$f.json
done
