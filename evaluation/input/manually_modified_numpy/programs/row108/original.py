arr = np.random.randint(-10, 10, size=N)
sorted_arr = sorted(list(set(arr)))
res = []
for ele in arr:
    res.append(sorted_arr.index(ele) + 1)
res
