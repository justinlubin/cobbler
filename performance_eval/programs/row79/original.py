def f(x):
    return x > 0

ruts = np.random.randint(-10, 10, size=N)
ruts_ok = []
for rut in ruts:
    ruts_ok.append(f(rut))
ruts_ok
