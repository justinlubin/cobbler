ages = np.arange(1, 50)
adult = []
for age in ages:
    if age > 18:
        adult.append(True)
    else:
        adult.append(False)
adult

# *** Interaction with np.where

ages = np.arange(1, 50)
adult = np.where(ages > 18, True, False)
adult