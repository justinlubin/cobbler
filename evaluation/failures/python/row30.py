text_len = []
for i in range(len(data_df)):
    row = data_df.iloc[i]
    text = row['rationale']
    text_len.append(len(text))
text_len
