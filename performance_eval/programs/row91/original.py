zp = np.random.randint(-10, 10, size=N)
ks = np.random.randint(-10, 10, size=N)

Mzs = 0
for i in range(zp.size):
    Mzs += ks[i] * zp[i]
Mzs = Mzs / ks.size
Mzs
