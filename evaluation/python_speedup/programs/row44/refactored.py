__array4411 = np.array(target_columns.itertuples())
target_type = list(np.add(np.add(np.full(len(__array4411), "."), np.vectorize(lambda x: x.filename)(__array4411)), np.vectorize(lambda x: x.listofcolumns)(__array4411)))
target_type
