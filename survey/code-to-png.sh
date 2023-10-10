#!/usr/bin/env bash

echo '```'"$1" > tmp1.md
cat tmp1.md "$2" > tmp2.md
echo -e '\n```' >> tmp2.md
pandoc tmp2.md -o "$2.html" --standalone --quiet --highlight-style=pygments
sed -i '' 's/<body>/<body style="margin: 0 !important; padding: 0 !important; max-width: 100% !important; font-size: 2em !important;">/g' "$2.html"
phantomjs render.js "$2"
convert "$2.untrimmed.png" -trim "$2.png"
rm "$2.html"
rm tmp1.md
rm tmp2.md
rm "$2.untrimmed.png"
