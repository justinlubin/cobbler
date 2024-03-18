tag_line = ''
for k in range(len(result)):
    tag_line += observation[k] + '/' + hidden_state[int(result[k])] + ' '
tag_line
