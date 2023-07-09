linx = np.random.randint(-10, 10, size=N)

f = lambda x: 1 / (1 + 9 * x ** 2)
liny = list(np.vectorize(f)(np.array(list(linx), dtype=object)))
liny
