symbols = {}

class Symbol:
    def __init__(self, s):
        self.name = s

    def __repr__(self):
        return self.name

    def __str__(self):
        return self.name


def intern(s):
    symbols.setdefault(s, Symbol(s))
    return symbols[s]


def symbol_name(sym):
    return sym.name
