values = np.random.randint(-10, 10, size = N)
__array4172 = np.array(list(values))
x = list(np.vectorize(int)(np.array(list(__array4172), dtype=object)))
x