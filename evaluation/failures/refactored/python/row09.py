nlist = []
for i in tlist:
    nlist.append(i.strip())
nlist

# *** Vectorized flexibility (semi-automatic)

f = lambda x: x.strip()
nlist = list(np.vectorize(f)(np.array(list(tlist), dtype=object)))
nlist

### Modified input:
# f = lambda x: x.strip()
# nlist = []
# for i in tlist:
#     nlist.append(f(i))
# nlist
