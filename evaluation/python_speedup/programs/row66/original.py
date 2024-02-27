from dataclasses import dataclass


@dataclass
class Text:
    text = ""


Temp = np.full(N, Text())

temp = []
for i in Temp:
    temp.append(i.text)
temp
