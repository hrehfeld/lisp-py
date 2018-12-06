from .base import Struct, concat

symbols = {}

Symbol, symbolp, (symbol_name, ), _symbol_setters = Struct('symbol', 'name')


def intern(s):
    symbols.setdefault(s, Symbol(s))
    return symbols[s]

gensym_counter = -1

def gensym(prefix='g'):
    if symbolp(prefix):
        prefix = symbol_name(prefix)
    global gensym_counter
    gensym_counter += 1
    return Symbol('__' + prefix + str(gensym_counter))
