Y_predicted = cnn.predict(X_test)
Y_predicted_arr = []
for i in range(100):
    Y_predicted_arr = np.append(Y_predicted_arr, np.argmax(Y_predicted[i]))
Y_predicted_arr
