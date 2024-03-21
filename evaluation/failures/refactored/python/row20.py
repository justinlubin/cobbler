counterOfMen = 0
for woman in friends.Woman:
    if not woman:
        counterOfMen += 1
counterOfMen

# *** Interaction with np.where

counterOfMen = np.sum(np.where(~np.array(friends.Woman), 1, 0))