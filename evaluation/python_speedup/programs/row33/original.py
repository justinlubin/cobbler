distances = np.random.randint(-10, 10, size = N)
times = np.random.randint(-10, 10, size = N)
speeds = []
for i in range(len(distances)):
    speeds.append(distances[i] / times[i])
speeds