lines = test_para.split('.')
test_lines = []
for line in lines:
    line = re.sub('[\\n\\"\\\']', '', line)
    test_lines.append(line)
test_lines

# *** Vectorized flexibility (semi-automatic)

f = lambda x: re.sub('[\\n\\"\\\']', '', x)
lines = test_para.split('.')
test_lines = list(np.vectorize(f)(np.array(list(lines), dtype=object)))
test_lines

### Modified input:
# f = lambda x: re.sub('[\\n\\"\\\']', '', x)
# lines = test_para.split('.')
# test_lines = []
# for line in lines:
#     test_lines.append(f(line))
# test_lines
