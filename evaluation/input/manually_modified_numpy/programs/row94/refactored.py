teste = list(np.random.randint(-10, 10, size=N))

len(teste)
len(list(set(teste)))
Generos = sorted(list(set(teste)))
Generos
n = list(np.vectorize(teste.count)(np.array(list(Generos), dtype=object)))
n
