X_f = X.flatten()
Frob_norm = 0
for i in X_f:
    Frob_norm += i**2
Frob_norm = np.sqrt(Frob_norm)
Frob_norm
