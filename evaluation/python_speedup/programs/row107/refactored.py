from dataclasses import dataclass


@dataclass
class Text:
    text = ""


soup_find_all = np.full(N, Text())

__array6521 = np.array(soup_find_all)
rating = list(np.vectorize(lambda x: x.text)(__array6521))
rating
