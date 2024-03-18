name_set = []
for i in range(len(name)):
    name_set.append(name[i])
    for j in range(len(name) - i - 1):
        two = name[i] + name[j + i + 1]
        name_set.append(two)
name_set
