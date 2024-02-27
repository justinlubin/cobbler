inputs = np.random.random(N)
weights = np.random.random(N)
b = 2
outputs = 0
for i in range(len(inputs)):
    outputs = outputs + inputs[i] * weights[i]
outputs = outputs + b
outputs
