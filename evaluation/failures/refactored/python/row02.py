top_five_green = sorted([(x, i) for i, x in enumerate(env_availability_score)], reverse=True)[:15]
top_five_green_origins = []
for item in top_five_green:
    top_five_green_origins.append(All_coordinates[item[1]])
top_five_green_origins

# *** Vectorized flexibility (semi-automatic)

f = lambda x: All_coordinates[x[1]]
top_five_green = sorted([(x, i) for i, x in enumerate(env_availability_score)], reverse=True)[:15]
top_five_green_origins = list(np.vectorize(f)(np.array(list(top_five_green), dtype=object)))
top_five_green_origins

### Modified input:
# f = lambda x: All_coordinates[x[1]]
# top_five_green = sorted([(x, i) for i, x in enumerate(env_availability_score)], reverse=True)[:15]
# top_five_green_origins = []
# for item in top_five_green:
#     top_five_green_origins.append(f(item))
# top_five_green_origins