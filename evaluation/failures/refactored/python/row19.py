lines = test_para.split('.')
test_lines = []
for line in lines:
    line = re.sub('[\\n\\"\\\']', '', line)
    test_lines.append(line)
test_lines
