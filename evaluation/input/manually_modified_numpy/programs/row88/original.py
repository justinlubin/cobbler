from dataclasses import dataclass


@dataclass
class Text:
    text = ""


titles = np.full(N, Text())

ls = []
for title in titles:
    ls.append(title.text)
ls
