sequence = 'HTHTTTTTHH'
model = {'H': 0.6, 'T': 0.4}
prob = 1
for char in sequence:
    prob *= model[char]
prob
