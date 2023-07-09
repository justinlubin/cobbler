teste = imdb['Genres'].str.cat(sep=', ').split(', ')
len(teste)
len(list(set(teste)))
Generos = sorted(list(set(teste)))
Generos
n = list(np.vectorize(teste.count)(np.array(list(Generos), dtype=object)))
n
