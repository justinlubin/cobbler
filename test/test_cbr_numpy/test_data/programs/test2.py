import numpy as np

def dot(x,y):
    out = 0
    for i in range(len(x)):
        out += x[i] * y[i]
    return out

def sum(x):
    out = 0
    for i in range(len(x)):
        out += x[i]
    return out

def mul(x,y):
    out = np.zeros(len(x))
    for i in range(len(x)):
        out[i] = x[i] * y[i]
    return out

