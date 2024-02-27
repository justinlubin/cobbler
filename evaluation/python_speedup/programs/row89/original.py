zp = np.random.randint(-10, 10, size=N)

Mz = 0
for i in range(zp.size):
    Mz += zp[i]
Mz = Mz / zp.size
Mz
