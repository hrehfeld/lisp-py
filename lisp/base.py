def native(f):
    return f

def sexps_str(form, indent=0):
    sexpr_print_operators = {
        quote_fun_name: quote_char
        , backquote_fun_name: backquote_char
        , backquote_eval_fun_name: backquote_eval_char
        , backquote_splice_fun_name: backquote_splice_char
    }

    assert(isinstance(indent, int)), indent
    def p(f):
        return ('  ' * indent + str(f) + '\n')

    r = ''
    if isinstance(form, list) or isinstance(form, tuple):
        is_simple = False
        for op, char in sexpr_print_operators.items():
           if is_named_operator(form, intern(op)):
               r += ' '.join([char + sexps_str(f) for f in form[1:]])
               is_simple = True
               break
        if not is_simple:
            r += p('(')
            for e in form:
                r += sexps_str(e, indent + 1)
            r += p(')')
    elif is_symbol(form):
        r += p(symbol_name(form))
    elif isinstance(form, dict):
        r += p('{')
        for e, v in form.items():
            r += sexps_str(e, indent + 1) + ': ' + sexps_str(v, indent + 1)
        r += p('}')
    elif isinstance(form, str):
        r += p('"%s"' % form)
    else:
        r += p(form)

    org = None
    while org != r:
        org = r
        r = r.replace('  ', ' ')
    if len(r) < 80:
        r = r.replace('\n', ' ')
            
    paren = '({'
    ws = [' ', '\n']
    for a in paren:
        for s in ws:
            r = r.replace(a + s, a)
    paren = '})'
    for a in paren:
        for s in ws:
            r = r.replace(s + a, a)
    return r
         

def ps(form):
    print(sexps_str(form))


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
        

@native
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
def is_named_operator(form, op):
    assert(is_symbol(op))
    return is_list(form) and form and is_symbol(form[0]) and form[0] == op


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
from .reader import quote_fun_name, backquote_fun_name, backquote_eval_fun_name, backquote_splice_fun_name, quote_char, backquote_char, backquote_eval_char, backquote_splice_char



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


make_env, is_env, (env_d, env_parent), _env_setters = defstruct('Env', 'd', 'parent')


def Env(parent=None):
    return make_env({}, parent)


def env_contains(env, k):
    assert(is_env(env))
    return k in env_d(env) or (env_parent(env) and env_contains(env_parent(env), k))


def env_get(env, k):
    assert(is_env(env))
    if k in env_d(env):
        return env_d(env)[k]
    elif env_parent(env):
        return env_get(env_parent(env), k)
    else:
        raise KeyError(k)


def env_containing_parent(env, k):
    assert(is_env(env))
    while env and k not in env_d(env):
        env = env_parent(env)
    return env


def env_def(env, k, v):
    assert(is_env(env))
    d = env_d(env)
    assert not k in d, '{k} in {d}'.format(k=k, d=env_d(env))
    print('~~~~~~~~env_def:', k, '=', sexps_str(v))
    env_d(env)[k] = v


def env_change(env, k, v):
    env = env_containing_parent(env, k) or env
    #print('        env_change:', k, '=', sexps_str(v), env_d(env).keys())
    env_d(env)[k] = v
