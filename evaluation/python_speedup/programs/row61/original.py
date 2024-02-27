from dataclasses import dataclass


@dataclass
class Text:
    text = ""


@dataclass
class A:
    a = Text()


addr_spans = np.full(N, A())

addresses = []
for span in addr_spans:
    addresses.append(span.a.text)
addresses
