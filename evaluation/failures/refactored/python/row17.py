counterAge = 0
for age in students.ages:
    if age % 2 == 0:
        counterAge += 1
counterAge

# *** Interaction with np.where

counterAge = np.sum(np.where(np.array(students.ages) % 2 == 0, 1, 0))