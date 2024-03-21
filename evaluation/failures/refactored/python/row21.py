nation = []
for dfs in dfss:
    nations = dfs['Location']
    for i in nations:
        nation.append(i)
nation

# *** Dictionary indexing + np.concat (2D array)

intermediate = [dfs['Location'] for dfs in dfss]
nation = list(np.concatenate(intermediate))
nation