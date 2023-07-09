arr = np.random.randint(-10, 10, size=N)
sorted_arr = sorted(list(set(arr)))
res = list(np.add(1, np.vectorize(sorted_arr.index)(np.array(list(arr), dtype=object))[:1]))
res
