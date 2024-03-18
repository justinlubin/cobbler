links = []
for elem in soup.find_all('a', attrs={'class': 'meta-title meta-title-link'}):
    links.append(elem.get('href'))
links
