se = np.random.randint(-10, 10, size = N)
lse = list(np.vectorize(np.log)(np.array(list(se), dtype=object)))
lse