def poi(a, b):
    return a


addresses = np.random.randint(-10, 10, size=N)

POIS = []
for address in addresses:
    POIS.append(poi(address, "canada"))
POIS
