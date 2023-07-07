values = range(N)
rootValues = list(np.vectorize(sqrt)(np.array(list(values), dtype=object)))
rootValues
