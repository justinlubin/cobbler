teste = list(np.random.randint(-10, 10, size=N))

len(teste)
len(list(set(teste)))
Generos = sorted(list(set(teste)))
Generos
n = []
for Genero in Generos:
    n.append(teste.count(Genero))
n
