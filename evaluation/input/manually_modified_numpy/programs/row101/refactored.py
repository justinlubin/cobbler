numbers = np.random.randint(-10, 10, size = N)
result = np.sum(np.vectorize(int)(np.array(list(numbers), dtype=object)))
result
