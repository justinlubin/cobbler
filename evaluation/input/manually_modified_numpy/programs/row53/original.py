columns_object = []
for column in columns:
    if X.dtypes[column] == 'object':
        columns_object.append(column)
columns_object
