values = np.random.randint(0, 10, size=N)
rootValues = list(np.vectorize(sqrt)(np.array(list(values), dtype=object)))
rootValues
