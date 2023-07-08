groceries = np.random.choice(['Eggs', 'Milk', 'Flour', 'Carrots', 'Napkins', 'Olive Oil'], size=N)
storage = []
for grocery_item in groceries[1:N-2]:
    storage.append(grocery_item)
storage
