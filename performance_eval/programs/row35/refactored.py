a = np.random.randint(-10, 10, size = N)
b = np.random.randint(-10, 10, size = N)
dot = np.sum(np.multiply(a, b[:len(a)]))
dot