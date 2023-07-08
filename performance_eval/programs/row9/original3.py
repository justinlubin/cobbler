import numpy as np
N = 10**5

final = np.random.randint(-10, 10, size=N)

mapped = []
for n in final:
    mapped.append(n)
mapped.sort()
mapped
