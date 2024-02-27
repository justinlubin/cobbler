a = np.random.randint(-10, 10, size = N)
b = np.random.randint(-10, 10, size = N)
dot = 0
for index in range(len(a)):
    dot += a[index] * b[index]
dot