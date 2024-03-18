tipos = ['jpg', 'png', 'PNG', 'JPG', 'JPEG', 'jpeg']
tipo_final = ''
for tipo in tipos:
    if tipo in str(img_bytes):
        tipo_final = tipo
tipo_final
