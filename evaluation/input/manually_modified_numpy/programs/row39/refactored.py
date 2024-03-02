values = np.random.randint(-10, 10, size = N)
__array4183 = np.array(list(values))
x = list(np.vectorize(int)(np.array(list(__array4183), dtype=object)))
x
