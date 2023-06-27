random_list = [1, 2, "three", 4.0, ["five"]]
foo = []
for item in random_list:
    foo.append(isinstance(item, str))
foo
