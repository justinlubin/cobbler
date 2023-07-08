linx = np.random.randint(1, 10, size=N)

f = lambda x: 1 / (1 + 9 * x ** 2)
liny = []
for i in range(len(linx)):
    liny.append(f(linx[i]))
liny
