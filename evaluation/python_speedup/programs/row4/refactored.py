planetas = np.full(N, "string")
short_planets = list(np.array(planetas)[(np.vectorize(len)(planetas) < 7)])
short_planets
