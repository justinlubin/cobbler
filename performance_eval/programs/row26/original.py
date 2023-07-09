tags = []
for tag in quotes[0].find_all('a', {'class': 'tag'}):
    tags.append(tag.text)
tags
