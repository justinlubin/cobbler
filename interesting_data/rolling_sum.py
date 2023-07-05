y = np.zeros(len(x) - window_size + 1)
for i in range(len(x) - window_size + 1):
    s = 0
    for j in range(window_size):
        s += x[i + j]
    y[i] = s
y
