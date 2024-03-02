from dataclasses import dataclass


@dataclass
class Text:
    text = ""


@dataclass
class H3:
    h3 = Text()


titles = np.full(N, H3())

title_list = []
for title in titles:
    title_list.append(title.h3.text)
title_list
