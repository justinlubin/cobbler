zp = np.random.randint(-10, 10, size=N)
ks = np.random.randint(-10, 10, size=N)

Ms = 0
for i in range(zp.size):
    Ms += ks[i]
Ms = Ms / ks.size
Ms
