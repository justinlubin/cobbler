input_0 = np.random.randint(-10, 10, size=N)
input_1 = np.random.randint(-10, 10, size=N)

sum = 0
for i in range(len(input_0)):
    sum += input_0[i] * input_1[i]
sum
