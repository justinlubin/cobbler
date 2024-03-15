maxlen = 0
for i in range(x_train.shape[0]):
    if len(x_train[i]) > maxlen:
        maxlen = len(x_train[i])
maxlen
