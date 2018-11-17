from .base import Struct, concat

symbols = {}

Symbol, symbolp, (symbol_name, ), _symbol_setters = Struct('symbol', 'name')


def intern(s):
    symbols.setdefault(s, Symbol(s))
    return symbols[s]

gensym_counter = -1

def gensym(prefix='g'):
    global gensym_counter
    gensym_counter += 1
    return Symbol(concat(prefix, str(gensym_counter)))
