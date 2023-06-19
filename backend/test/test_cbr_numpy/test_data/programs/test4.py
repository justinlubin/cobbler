x = np.arange(-5, 5)
y = np.zeros(len(x))
for i in range(len(x)):
    if x[i] > 0:
        y[i] = 1
    else:
        y[i] = -1
y
