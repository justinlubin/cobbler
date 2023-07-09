x = np.random.randint(-10, 10, size=N)
squares = np.multiply(x, x)
sqaures_under_10 = []
for sqaure in sqaures:
    if sqaure < 10:
        sqaures_under_10.append(sqaure)
sqaures_under_10
