lse = np.random.randint(-10, 10, size = N)
mlse = list(np.vectorize(np.mean)(np.array(list(lse), dtype=object)))
mlse