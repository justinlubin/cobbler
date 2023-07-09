columns_object = list(np.array(columns)[(np.vectorize(__memberAccess)(dtypes, X)[columns] == "object")])
columns_object
