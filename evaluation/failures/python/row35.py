l = []
for fn in files:
    l.append(fn.split('-')[1] + '39')
l[1] = 'bdd_oia_head1_39'
l[-2] = 'whole_attention_resnet_39'
l[-1] = 'whole_attention_v1_39'
l
