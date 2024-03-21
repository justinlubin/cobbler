a = []
for l in data_words:
    a.append(len(l))
a

# *** Vectorized flexibility (semi-automatic)

len_ = len
a = list(np.vectorize(len_)(np.array(list(data_words), dtype=object)))
a

### Modified input:
# len_ = len
# a = []
# for l in data_words:
#     a.append(len_(l))
# a
