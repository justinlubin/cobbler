from dataclasses import dataclass


@dataclass
class Text:
    text = ""


titles = np.full(N, Text())

ls = list(np.vectorize(lambda x: x.text)(titles))
ls
