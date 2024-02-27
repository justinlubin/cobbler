def unidecode(x):
    return x


escuelas = np.random.randint(-10, 10, size=N)

unaccented_schools = list(
    np.vectorize(unidecode)(np.array(list(escuelas), dtype=object))
)
unaccented_schools
