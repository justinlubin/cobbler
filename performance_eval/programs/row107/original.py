from dataclasses import dataclass


@dataclass
class Text:
    text = ""


soup_find_all = np.full(N, Text())

rating = []
for i in soup_find_all:
    rating.append(i.text)
rating
