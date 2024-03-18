clean_text = text
for bad_char in string.punctuation:
    clean_text = clean_text.replace(bad_char, '')
clean_text
