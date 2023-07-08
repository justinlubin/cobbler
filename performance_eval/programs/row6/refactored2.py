import numpy as np
N = 10**3

numbers = np.random.randint(-10, 10, size=N)

tens_ls = list(np.multiply(numbers, 10))
tens_ls
