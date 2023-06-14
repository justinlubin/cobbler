import numpy as np

def matmul(x,y):
    out = np.zeros(len(x),len(y[0]))
    for i in range(len(x)):
        for j in range(len(y[0])):
            dot = 0
            for k in range(len(y)):
                dot += x[i][k] * y[k][j]
            out[i][j] = dot
    return out

x = np.zeros(2,2)
y = np.zeros(2,2)
i = 1
x[0][0] = 3
y[1][0] = 5
x[i-1][i*1]=4
z = matmul(x,y)

