russian_ingredients = set()
for recipe in recipes:
    if recipe['cuisine'] == 'russian':
        russian_ingredients = russian_ingredients.union(set(recipe['ingredients']))
russian_ingredients
