res = []
for text in content:
    for pattern in patterns:
        if re.findall(pattern, text):
            res.append(text)
res

# *** Cannot refactor (2D jagged arrays)