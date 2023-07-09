rating = []
for i in soup.find_all('td', class_='table-body__cell u-text-right rating'):
    rating.append(i.text)
rating
