target_type = []
for row in target_columns.itertuples():
    target_type.append(row.hi.filename + "." + row.listofcolumns)
target_type
