sample_images = []
for i in range(0, 5):
    record = pick_one_record(train_df, i)
    sample_images.append(record)
print('.. sample_images')
sample_images
