__array3747 = np.array(quotes[0].find_all('a', {'class': 'tag'}))
tags = list(np.vectorize(lambda x: x.text)(__array3747))
tags
