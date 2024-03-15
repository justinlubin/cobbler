age_range = []
for i in range(len(list(grouped['age']['mean']))):
    val = list(grouped['age']['max'])[i] - list(grouped['age']['min'])[i]
    if val > 0:
        age_range.append(str(list(grouped['age']['min'])[i]) + '-' + str(list(grouped['age']['max'])[i]))
    else:
        age_range.append(str(list(grouped['age']['max'])[i]))
age_range
