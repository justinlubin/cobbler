distances = np.random.randint(-10, 10, size = N)
times = np.random.randint(-10, 10, size = N)
speeds = list(np.divide(distances, times[:len(distances)]))
speeds