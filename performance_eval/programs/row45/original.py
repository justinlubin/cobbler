explained_variance_values_values = np.randon.randint(-10, 10, size=N)
total = 0
for i in sorted(explained_variance_values_values, reverse=True):
    total += i
total
