from dataclasses import dataclass


@dataclass
class Text:
    text = ""


Temp = np.full(N, Text())

temp = list(np.vectorize(lambda x: x.text)(Temp))
temp
