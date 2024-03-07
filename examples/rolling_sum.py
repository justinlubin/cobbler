y = np.zeros(len(x) - WINDOW_SIZE + 1)
for i in range(len(y)):
    s = 0
    for j in range(WINDOW_SIZE):
        s += x[i + j]
    y[i] = s
y
