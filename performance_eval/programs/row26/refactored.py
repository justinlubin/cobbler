from dataclasses import dataclass


@dataclass
class Text:
    text = ""


find_all = np.full(N, Text())

__array3747 = np.array(find_all)
tags = list(np.vectorize(lambda x: x.text)(__array3747))
tags
