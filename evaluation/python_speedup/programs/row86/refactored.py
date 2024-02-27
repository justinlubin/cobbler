enc_flag = np.random.choice(['1','2','3','a','b','c'], size = N)

int_enc_flag = list(np.vectorize(int, excluded={2})(np.array(list(enc_flag), dtype=object), 16))
int_enc_flag
