from dataclasses import dataclass


@dataclass
class Text:
    text = ""


@dataclass
class A:
    a = Text()


addr_spans = np.full(N, A())

addresses = list(np.vectorize(lambda x: x.a.text)(addr_spans))
addresses
