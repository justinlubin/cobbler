import string
myURLS = np.random.choice(list(string.ascii_lowercase), size=N)

baseURL = 'http://www.importantsite.com/'
full_URLS = list(np.add(np.full(len(myURLS), baseURL), myURLS))
full_URLS
s