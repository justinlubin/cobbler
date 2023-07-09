def poi(a, b):
    return a


addresses = np.random.randint(-10, 10, size=N)

POIS = list(
    np.vectorize(poi, excluded={2})(np.array(list(addresses), dtype=object), "canada")
)
POIS
