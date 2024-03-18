train_files = []
for dir in full_img_dirs:
    files = os.listdir(dir)
    for imgfile in files:
        fullpath = dir + '/' + imgfile
        train_files.append(fullpath)
train_files
