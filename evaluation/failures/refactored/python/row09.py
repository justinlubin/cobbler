# nlist = []
# for i in tlist:
#     nlist.append(i.strip())
# nlist

# *** Vectorized method call (semi-automatic)

f = lambda x: strip(x)
nlist = list(np.vectorize(f)(np.array(list(tlist), dtype=object)))
nlist

### Modified input:
# f = lambda x: strip(x)
# nlist = []
# for i in tlist:
#     nlist.append(f(i))
# nlist