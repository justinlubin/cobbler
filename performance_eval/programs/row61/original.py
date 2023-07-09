addr_spans = soup.find_all('span', class_='addr')
addresses = []
for span in addr_spans:
    addresses.append(span.a.text)
addresses
