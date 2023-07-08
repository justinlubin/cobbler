y = range(1, N+1)

ln_y = list(np.vectorize(np.log)(np.array(list(y), dtype=object)))
ln_y
