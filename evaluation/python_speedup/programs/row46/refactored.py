mse = np.random.randint(-10, 10, size = N)
rmse = list(np.vectorize(np.sqrt)(np.array(list(mse), dtype=object)))
rmse