symbols = {}

SYMBOL = '__SYMBOL'

def Symbol(s):
    assert(s)
    return (SYMBOL, s)


def intern(s):
    symbols.setdefault(s, Symbol(s))
    return symbols[s]


def symbol_name(sym):
    return sym[1]


def symbolp(sym):
    return isinstance(sym, tuple) and len(sym) == 2 and sym[0] is SYMBOL


