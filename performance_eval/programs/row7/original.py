team1 = np.random.randint(-10, 10, size=N)
team2 = np.random.randint(-10, 10, size=N)

total_team = []
for i in range(len(team1)):
    total_team.append(team1[i] + team2[i])
total_team
