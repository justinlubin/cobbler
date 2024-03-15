prohibited_paths = []
for statement in r.text.split('\n'):
    if 'Disallow:' in statement:
        current_path = statement.strip('Disallow: ')
        prohibited_paths.append('https://finance.yahoo.com' + current_path)
prohibited_paths
