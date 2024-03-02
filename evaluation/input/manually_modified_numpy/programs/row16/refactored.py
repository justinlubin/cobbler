y = np.random.randint(1, 20, size=N)

ln_y = list(np.vectorize(np.log)(np.array(list(y), dtype=object)))
ln_y
