symbols = {}

class Symbol:
    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s

    def __str__(self):
        return self.s


def intern(s):
    symbols.setdefault(s, Symbol(s))
    return symbols[s]


def symbol_name(sym):
    return sym.s
