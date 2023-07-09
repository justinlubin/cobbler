POIS = list(np.vectorize(poi, excluded={2})(np.array(list(addresses), dtype=object), "canada"))
POIS
