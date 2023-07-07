values = range(N)
rootValues = list(np.vectorize(np.sqrt)(np.array(list(values), dtype=object)))
rootValues
