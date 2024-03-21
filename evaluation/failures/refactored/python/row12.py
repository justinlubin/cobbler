data_sort = data[data[:, 1].argsort()]
outlier_bonus = []
for i in range(-5, 0):
    outlier_bonus.append(data_sort[i][1])
outlier_bonus

# *** Multi-dimensional array

data_sort = data[data[:, 1].argsort()]
outlier_bonus = data_sort[np.arange(-5, 0), 1]
outlier_bonus