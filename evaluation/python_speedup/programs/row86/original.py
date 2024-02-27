enc_flag = np.random.choice(['1','2','3','a','b','c'], size = N)

int_enc_flag = []
for i in enc_flag:
    int_enc_flag.append(int(i, 16))
int_enc_flag
