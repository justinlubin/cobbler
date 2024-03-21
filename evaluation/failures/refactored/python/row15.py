new_list = []
for element in l:
    new_value = element * 10
    new_list.append(new_value)
new_list

# *** Uses temporary variable (semi-automatic)

new_list = list(np.multiply(l, 10))
new_list

### Modified input:
# new_list = []
# for element in l:
#     new_list.append(element * 10)
# new_list