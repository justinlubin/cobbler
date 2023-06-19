array_out = np.zeros_like(array_a)
for i in range(len(array_a)):
    array_out[i] = array_a[i] + array_b[i]
array_out
