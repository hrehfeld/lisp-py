def native(f):
    return f

TYPE = '__type'
TYPE_T = '__type_t'

def make_dict(*args):
    assert (len(args) % 2 == 0), args
    kwargs = {}
    for i in range(0, len(args), 2):
        k = args[i]
        v = args[i + 1]
        kwargs[k] = v
    return kwargs
        

def defstruct(name_str, *field_names):
    for n in field_names:
        assert(isinstance(n, str))
    type = make_dict(TYPE, TYPE_T, 'name', name_str, 'fields', field_names)

    def constructor(*values):
        assert(len(field_names) == len(values))
        r = {TYPE: type}
        for k, v in zip(field_names, values):
            r[k] = v
        return r

    def is_instance(obj):
        return isinstance(obj, dict) and obj.get(TYPE, None) == type

    getters = []
    setters = []
    for ifield, field in enumerate(field_names):
        fname = '%s-%s' % (name_str, (field))
        def get(struct):
            assert(is_instance(struct)), str(struct) + repr(struct)
            return struct[field]
        getters += [get]

        def set(struct, value):
            struct[field] = value
            return struct
        setters += [set]

    return constructor, is_instance, getters, setters


def is_struct(obj):
    return isinstance(obj, dict) and TYPE in obj

from .symbol import is_symbol, symbol_name, intern

def concat(*rest):
    r = ''
    for e in rest:
        r += e
    return r

MACRO = '__macro'
SPECIAL_FORM = '__special'


def is_special_form(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == SPECIAL_FORM


def special_form(f):
    if not is_callable(f):
        raise Exception('%s is not callable' % (f))
    return (SPECIAL_FORM, f)


def special_form_get_fun(f):
    return f[1]


def is_macro(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == MACRO


def Macro(f):
    if not is_callable(f):
        raise Exception('%s is not callable' % (f))

    return (MACRO, f)


def macro_get_fun(macro):
    return macro[1]




def is_int(v):
    return isinstance(v, int)


def is_num(f):
    return isinstance(f, int) or isinstance(f, float)


def is_str(f):
    return isinstance(f, str)


def is_atom(form):
    return is_num(form) or is_str(form) or is_keyword(form) or is_symbol(form) or (is_list(form) and not len(form))

from .reader import keyword_start

def __keyword(s):
    if is_symbol(s):
        s = symbol_name(s)
    assert(is_str(s))
    return intern(keyword_start + s)


@native
def keyword(*args, **kwargs):
    return __keyword(*args, **kwargs)
    
    
def keyword_name(s):
    assert(is_keyword(s)), s
    return symbol_name(s)[len(keyword_start):]

def is_keyword(e):
    return is_symbol(e) and symbol_name(e).startswith(keyword_start)


def is_special_keyword(e):
    return is_symbol(e) and symbol_name(e).startswith('&')


def is_list(e):
    return isinstance(e, list)


def __length(e):
    assert(isinstance(e, list))
    return len(e)


@native
def length(*args, **kwargs):
    return __length(*args, **kwargs)
    
    
def is_callable(e):
    return callable(e)
