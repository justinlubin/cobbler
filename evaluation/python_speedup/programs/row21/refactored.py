inputs = np.random.random(N)
weights = np.random.random(N)
b = 2
outputs = np.sum(np.multiply(inputs, weights[:len(inputs)]))
outputs = outputs + b
outputs
