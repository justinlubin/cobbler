__array6521 = np.array(soup.find_all('td', class_='table-body__cell u-text-right rating'))
rating = list(np.vectorize(lambda x: x.text)(__array6521))
rating
