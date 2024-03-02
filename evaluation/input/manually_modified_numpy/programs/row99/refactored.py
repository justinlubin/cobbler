from dataclasses import dataclass


@dataclass
class Text:
    text = ""


@dataclass
class H3:
    h3 = Text()


titles = np.full(N, H3())

title_list = list(np.vectorize(lambda x: x.h3.text)(titles))
title_list
