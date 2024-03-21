tipos = ['jpg', 'png', 'PNG', 'JPG', 'JPEG', 'jpeg']
tipo_final = ''
for tipo in tipos:
    if tipo in str(img_bytes):
        tipo_final = tipo
tipo_final

# *** Nontrivial

tipos = ['jpg', 'png', 'PNG', 'JPG', 'JPEG', 'jpeg']
tipo_final = tipos[np.argwhere(np.char.find(img_bytes, tipos) > 0)[-1][0]]
tipo_final