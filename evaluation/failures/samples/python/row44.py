panel_size = 1453
tvds = []
for i in range(5000):
    temp = jury_2
    temp['Rand_Sample'] = np.random.multinomial(1453, list(temp.Eligible)) / 1453
    tvds.append(tvd(temp.Eligible, temp.Rand_Sample))
tvds
