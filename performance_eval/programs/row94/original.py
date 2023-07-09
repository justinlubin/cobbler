teste = imdb['Genres'].str.cat(sep=', ').split(', ')
len(teste)
len(list(set(teste)))
Generos = sorted(list(set(teste)))
Generos
n = []
for Genero in Generos:
    n.append(teste.count(Genero))
n
