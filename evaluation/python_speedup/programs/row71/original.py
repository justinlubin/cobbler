def unidecode(x):
    return x


escuelas = np.random.randint(-10, 10, size=N)

unaccented_schools = []
for sch in escuelas:
    unaccented_schools.append(unidecode(sch))
unaccented_schools
