planetas = np.full(N, "string")
short_planets = []
for planet in planetas:
    if len(planet) < 7:
        short_planets.append(planet)
short_planets
