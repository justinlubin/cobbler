total_num_edges = 0
for hero in undir_hero_map.keys():
    edge_length = len(undir_hero_map[hero])
    total_num_edges += edge_length
total_num_edges /= 2
total_num_edges
