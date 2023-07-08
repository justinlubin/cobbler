def f(x):
    return x > 0

ruts = np.random.randint(-10, 10, size=N)
ruts_ok = list(np.vectorize(f)(np.array(list(ruts), dtype=object)))
ruts_ok
