import numpy as np
N = 10**7

numbers = np.random.randint(-10, 10, size=N)

tens_ls = []
for number in numbers:
    tens_ls.append(number * 10)
tens_ls
