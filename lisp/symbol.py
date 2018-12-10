from .base import defstruct, py_bind_env

symbols = {}

py_bind_env(['symbol', 'is_symbol', ['symbol_name'], '_'], defstruct('symbol', 'name'), __name__)
# Symbol, is_symbol, (symbol_name, ), _symbol_setters = defstruct('symbol', 'name')


def intern(s):
    if s not in symbols:
        symbols[s] = symbol(s)
    return symbols[s]

gensym_counter = -1

def gensym(prefix='g'):
    if is_symbol(prefix):
        prefix = symbol_name(prefix)
    global gensym_counter
    gensym_counter += 1
    return symbol('__' + prefix + str(gensym_counter))
