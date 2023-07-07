input_0 = np.random.randint(-10, 10, size=N)
input_1 = np.random.randint(-10, 10, size=N)

sum = np.sum(np.multiply(input_0, input_1[:len(input_0)]))
sum
