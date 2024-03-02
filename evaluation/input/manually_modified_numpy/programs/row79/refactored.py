from dataclasses import dataclass


@dataclass
class Text:
    text = ""


titles = np.full(N, Text())

title_list = list(np.vectorize(lambda x: x.text)(titles))
title_list
