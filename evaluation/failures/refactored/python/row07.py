rounds = []
for i in range(10):
    rounds.append(IPDGame(STfT, NP, 50))
rounds

# *** Intermediate (semi-automatic)

intermediate = IPDGame(STfT, NP, 50)
rounds = list(np.full(10, intermediate))
rounds

### Modified input:
# intermediate = IPDGame(STfT, NP, 50)
# rounds = []
# for i in range(10):
#     rounds.append(intermediate)
# rounds