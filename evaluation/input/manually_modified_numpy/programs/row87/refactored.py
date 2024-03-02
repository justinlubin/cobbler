G = np.random.randint(-10, 10, size=(N, 3))
SumG = list(np.vectorize(sum)(np.array(list(G), dtype=object)))
SumG
