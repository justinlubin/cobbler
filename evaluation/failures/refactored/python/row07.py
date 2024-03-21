rounds = []
for i in range(10):
    rounds.append(IPDGame(STfT, NP, 50))
rounds

# *** np.repeat

# Assumes IPDGame does not return something implicitly convertible as a NumPy array
rounds = list(np.repeat(IPDGame(STfT, NP, 50), 10))
rounds