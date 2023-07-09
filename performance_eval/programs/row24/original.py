import string
myURLS = np.random.choice(list(string.ascii_lowercase), size=N)

baseURL = 'http://www.importantsite.com/'
full_URLS = []
for myURL in myURLS:
    full_URLS.append(baseURL + myURL)
full_URLS
