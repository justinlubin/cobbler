nation = []
for dfs in dfss:
    nations = dfs['Location']
    for i in nations:
        nation.append(i)
nation

# *** Dictionary indexing + np.concatenate (list of arrays)

intermediate = [dfs['Location'] for dfs in dfss]
nation = list(np.concatenate(intermediate))
nation