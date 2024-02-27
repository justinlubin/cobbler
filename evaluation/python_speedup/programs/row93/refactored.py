n_squares_sqrt = int(np.sqrt(N))
n_squares = n_squares_sqrt * n_squares_sqrt
board_22 = np.power(np.full(n_squares, 2), np.arange(n_squares))
board_22 = board_22.reshape(n_squares_sqrt, n_squares_sqrt)
board_22
