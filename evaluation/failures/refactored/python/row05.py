sequence = 'HTHTTTTTHH'
model = {'H': 0.6, 'T': 0.4}
prob = 1
for char in sequence:
    prob *= model[char]
prob

# *** Dictionary indexing (semi-automatic)

sequence = 'HTHTTTTTHH'
model = {'H': 0.6, 'T': 0.4}
intermediate = [model[s] for s in sequence]
prob = np.prod(intermediate)
prob

### Modified input:
# sequence = 'HTHTTTTTHH'
# model = {'H': 0.6, 'T': 0.4}
# intermediate = [model[s] for s in sequence]
# prob = 1
# for i in intermediate:
#     prob *= i
# prob