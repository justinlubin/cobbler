import numpy as np

def dot(x,y):
    out = 0.0
    for i in range(len(x)):
        out += x[i] * y[i]

def sum(x):
    out = 0.0
    for i in range(len(x)):
        out += x[i]

def mul(x,y):
    out = np.zeros(len(x))
    for i in range(len(x)):
        out[i] = x[i] * y[i]

