addr_spans = soup.find_all('span', class_='addr')
addresses = list(np.vectorize(lambda x: x.a.text)(addr_spans))
addresses
