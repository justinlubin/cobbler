palavra = '!@#$%' * N
cods = list(np.vectorize(ord)(np.array(list(palavra), dtype=object)))
cods
