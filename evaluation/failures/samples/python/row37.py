test = 0
for item in predicted_pos:
    if item[1] not in true_pos:
        test += 1
test
