a = np.random.randint(-10, 10, size = N)
b = np.random.randint(-10, 10, size = N)
dot = 0
for i in range(len(a)):
    dot += a[i] * b[i]
dot