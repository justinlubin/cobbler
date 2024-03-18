def COS_SIM(a, b):
    dot_product = np.dot(a, b)
    norm_a = np.linalg.norm(a)
    norm_b = np.linalg.norm(b)
    return dot_product / (norm_a * norm_b + 1)
mat = COUNT_MAT.toarray()
res = []
for i in range(len(mat)):
    curr = []
    for j in range(len(mat)):
        if i != j:
            curr.append(round(COS_SIM(mat[i], mat[j])))
        else:
            curr.append(1)
    res.append(curr)
res = np.reshape(res, [len(mat), len(mat)])
res
