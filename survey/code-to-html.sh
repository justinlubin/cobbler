#!/usr/bin/env bash

echo '```'"$1" > tmp1.md
cat tmp1.md "$2" > tmp2.md
echo -e '\n```' >> tmp2.md
pandoc tmp2.md -o "$2.html" --standalone --quiet --highlight-style=pygments
sed -i '' '1,7d' "$2.html"
sed -i '' '$d' "$2.html"
sed -i '' '$d' "$2.html"
sed -i '' '/<\/head>/d' "$2.html"
sed -i '' '/<body>/d' "$2.html"
rm tmp1.md
rm tmp2.md
