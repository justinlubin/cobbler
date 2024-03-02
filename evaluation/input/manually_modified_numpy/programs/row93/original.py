n_squares_sqrt = int(np.sqrt(N))
n_squares = n_squares_sqrt * n_squares_sqrt
board_22 = np.zeros(n_squares)
for i in range(n_squares):
    board_22[i] = 2**i
board_22 = board_22.reshape(n_squares_sqrt, n_squares_sqrt)
board_22
