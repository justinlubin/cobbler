import numpy as np
N = 10

def func(x):
    return x * x + x + 100

lista = []
for i in range(N):
    lista.append(func(i))
lista
