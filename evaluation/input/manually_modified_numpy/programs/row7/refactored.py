team1 = np.random.randint(-10, 10, size=N)
team2 = np.random.randint(-10, 10, size=N)

total_team = list(np.add(team2[:len(team1)], team1))
total_team
