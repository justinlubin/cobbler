x = np.random.randint(-10, 10, size=N)
squares = np.multiply(x, x)
sqaures_under_10 = list(np.array(sqaures)[(sqaures < 10)])
sqaures_under_10
