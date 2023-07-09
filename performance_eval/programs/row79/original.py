from dataclasses import dataclass


@dataclass
class Text:
    text = ""


titles = np.full(N, Text())

title_list = []
for title in titles:
    title_list.append(title.text)
title_list
