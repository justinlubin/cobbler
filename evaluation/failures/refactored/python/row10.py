age_range = []
for i in range(len(list(grouped['age']['mean']))):
    val = list(grouped['age']['max'])[i] - list(grouped['age']['min'])[i]
    if val > 0:
        age_range.append(str(list(grouped['age']['min'])[i]) + '-' + str(list(grouped['age']['max'])[i]))
    else:
        age_range.append(str(list(grouped['age']['max'])[i]))
age_range

# *** Nontrivial (string datatype conversions, np.where interaction, deep string addition)

means = grouped['age']['mean']
mins = grouped['age']['min']
maxes = grouped['age']['max']
age_range = list(np.where(maxes - mins > 0, np.char.add(np.char.add(mins.astype(str), "-"), maxes.astype(str)), maxes.astype(str)))
age_range