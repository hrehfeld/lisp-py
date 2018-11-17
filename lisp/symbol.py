from .base import Struct

symbols = {}

Symbol, symbolp, (symbol_name, ), _symbol_setters = Struct('symbol', 'name')


def intern(s):
    symbols.setdefault(s, Symbol(s))
    return symbols[s]


