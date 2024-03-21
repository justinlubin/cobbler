sum_price = 0
for i in range(0, line_count, 1):
    sum_price = sum_price + price[i]
sum_price

# *** range arguments (semi-automatic)

sum_price = np.sum(price)
sum_price

### Modified input:
# sum_price = 0
# for i in range(line_count):
#     sum_price = sum_price + price[i]
# sum_price