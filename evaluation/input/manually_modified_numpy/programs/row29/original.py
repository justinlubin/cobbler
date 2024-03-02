def coordinates_to_uv(coord_array):
    x, y = coord_array
    u, v = x ** 2 + y ** 2, x * y - 1
    return [u, v]

coordinates = np.random.randint(-10, 10, size=(N, 2))
uv_values = []
for coords in coordinates:
    uv_values.append(coordinates_to_uv(coords))
uv_values
