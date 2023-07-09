n_squares = N
board_22 = np.zeros(n_squares)
for i in range(n_squares):
    board_22[i] = 2 ** i
board_22 = board_22.reshape(2, 2)
board_22
