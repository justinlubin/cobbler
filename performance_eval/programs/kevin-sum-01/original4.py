import numpy as np

N = 10**7

x = np.random.randint(-10, 10, size=N)
the_sum = 0
for value in x:
    the_sum = the_sum + value
the_sum
