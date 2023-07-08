numbers = random.randint(0, 10, size=N)
numbers_under_4 = []
for number in numbers:
    if number < 4:
        numbers_under_4.append(number)
numbers_under_4
