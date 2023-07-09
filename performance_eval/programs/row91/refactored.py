zp = np.random.randint(-10, 10, size=N)
ks = np.random.randint(-10, 10, size=N)

Mzs = np.sum(np.multiply(np.multiply(np.ones(zp.size), zp[: zp.size]), ks[: zp.size]))
Mzs = Mzs / ks.size
Mzs
