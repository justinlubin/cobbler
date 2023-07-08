import numpy as np
N = 10**7

def func(x):
    return x * x + x + 100

__array2798 = np.array(range(N))
lista = list(np.vectorize(func)(np.array(list(__array2798), dtype=object)))
lista
