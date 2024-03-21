profitratio = []
for i in range(0, len(sales)):
    profitratio.append(profit[i] / sales[i])
profitratio

# *** range arguments (semi-automatic)

profitratio = list(np.divide(profit[:len(sales)], sales))
profitratio

### Modified input:
# profitratio = []
# for i in range(len(sales)):
#     profitratio.append(profit[i] / sales[i])
# profitratio