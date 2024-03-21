numbers = [1, 2, 3, 4, 5]
squares_under_10 = []
for number in numbers:
    if number * number < 10:
        squares_under_10.append(number * number)
squares_under_10

# *** Intermediate (semi-automatic)

numbers = [1, 2, 3, 4, 5]
intermediate = np.power(numbers, 2)
squares_under_10 = list(np.array(intermediate)[(intermediate < 10)])
squares_under_10

### Modified input:
# numbers = [1, 2, 3, 4, 5]
# intermediate = np.power(numbers, 2)
# squares_under_10 = []
# for i in intermediate:
#     if i < 10:
#         squares_under_10.append(i)
# squares_under_10