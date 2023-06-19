d = np.zeros(a.size - 1)
for i in range(len(a) - 1):
    d[i] = a[i + 1] - a[i]
d
