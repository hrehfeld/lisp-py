import sys

def native(f):
    return f


def set_limit():
    pass


@native
def set_limit():
    sys.setrecursionlimit(16384 * 4)


set_limit()


def get_interpreter_meta_level():
    return __interpreter_meta_level


@native
def get_interpreter_meta_level():
    return 0


@native
def sexps_str(form, indent=0, seen=None, full=False):
    if seen is None:
        seen = set()
    sexpr_print_operators = {
        quote_fun_sym: quote_char
        , backquote_fun_sym: backquote_char
        , backquote_eval_fun_sym: backquote_eval_char
        , backquote_splice_fun_sym: backquote_splice_char
    }

    def is_seen(x):
        return not full and id(x) in seen

    def add_seen(x):
        seen.add(id(x))

    assert(is_int(indent)), indent
    def p(f):
        return (''.join(['  ' for i in range(indent)]) + str(f) + '\n')

    values = {}
    values[True] = 'true'
    values[False] = 'false'
    values[None] = 'nil'

    r = ''
    if is_env(form):
        if is_seen(form):
            r +=  '<cyclic>'
        else:
            add_seen(form)

            r += p('(Env {s})'.format(s=''.join([sexps_str(f, indent + 1, seen, full) for f in env_d(form).keys()]) + '<parents...>'))
    
    elif is_list(form) or is_tuple(form):
        if is_seen(form):
            r += p('<cyclic list>')
        else:
            add_seen(form)

            is_simple = False
            for op, char in sexpr_print_operators.items():
                if is_list(form) and form and form[0] is op:
                    assert(len(form) == 2)
                    r += p(char + ' '.join([sexps_str(f, indent + 1, seen, full) for f in form[1:]]).strip())
                    is_simple = True
                    break
            if not is_simple:
                r += p('(')
                for e in form:
                    r += sexps_str(e, indent + 1, seen, full)
                r += p(')')
    elif is_struct(form):
        t = form[TYPE]
        fields = t['fields']
        fields = [':{f} {v}'.format(f=f, v=sexps_str(form[f], seen=seen, full=full)) for f in fields]
        r += p('({t} {fields})'.format(t=t['__name__'], fields=' '.join(fields)))
    elif is_symbol(form):
        r += p(symbol_name(form))
    elif is_dict(form):
        if is_seen(form):
            r += p('<cyclic dict>')
        else:
            add_seen(form)

            r += p('(dict')
            for k, v in form.items():
                assert is_str(k), k
                r += p(':' + k + ' ' + sexps_str(v, indent + 1, seen, full))
                #r += ':' + sexps_str(k, indent + 1, seen, full) + ' ' + sexps_str(v, indent + 1, seen, full)
            r += p(')')
    elif is_str(form):
        #TODO
        if not full and len(form) > 30:
            form = form[:30] + '[..]'
        r += p('"%s"' % form)
    elif is_num(form):
        r += p(str(form))
    elif form in values:
        r += p(values[form])
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


def debug(*args):
    if get_interpreter_meta_level() > 0:
        print(*[a() if is_callable(a) else a for a in args])
        

TYPE = '__type'
TYPE_T = '__type_t'

@native
def is_int(v):
    return isinstance(v, int)


@native
def is_float(v):
    return isinstance(v, float)


@native
def is_str(f):
    return isinstance(f, str)


def is_num(f):
    return is_int(f) or is_float(f)


@native
def is_tuple(v):
    return isinstance(v, tuple)


@native
def is_dict(d):
    return isinstance(d, dict) and not is_struct(d)


@native
def __defstruct(name_str, *field_names):
    assert(is_str(name_str)), name_str
    for n in field_names:
        assert(isinstance(n, str))
    type_marker = {
        '__name__': name_str
        , 'fields': field_names
    }

    def constructor(*values):
        assert(len(field_names) == len(values))
        r = {}
        r[TYPE] = type_marker
        for k, v in zip(field_names, values):
            r[k] = v
        return r

    def is_instance(obj):
        return isinstance(obj, dict) and TYPE in obj and obj[TYPE] == type_marker

    getters = []
    setters = []
    for ifield, field in enumerate(field_names):
        fname = '%s-%s' % (name_str, (field))
        def make_getter(field):
            def get(struct):
                assert(is_instance(struct)), '{T}: {val}'.format(val=sexps_str(struct), T=name_str)
                return struct[field]
            return get
        getters += [make_getter(field)]

        def set(struct, value):
            struct[field] = value
            return struct
        setters += [set]

    return constructor, is_instance, getters, setters


@native
def is_struct(obj):
    return isinstance(obj, dict) and TYPE in obj

# from .symbol import is_symbol, symbol_name, intern

def is_named_operator(form, op):
    assert(is_symbol(op))
    return is_list(form) and form and is_symbol(form[0]) and form[0] == op


special_form, is_special_form, (special_form_fun,), _ = __defstruct('special-form', 'fun')

macro, is_macro, (macro_fun,), _ = __defstruct('macro', 'fun')


def is_atom(form):
    return is_num(form) or is_str(form) or is_keyword(form) or is_symbol(form) or (is_list(form) and not len(form))


def keyword(s):
    assert is_str(s), sexps_str(s)
    return intern(keyword_start + s)


def keyword_name(s):
    assert(is_keyword(s)), s
    return symbol_name(s)[len(keyword_start):]


def is_keyword(e):
    return is_symbol(e) and symbol_name(e).startswith(keyword_start)


def is_special_keyword(e):
    return is_symbol(e) and symbol_name(e).startswith('&')


@native
def is_iterable(o):
    try:
        iter(o)
    except TypeError:
        return False
    return True


@native
def is_list(e):
    return isinstance(e, list)


@native
def length(l):
    return len(l)


@native
def as_list(arg):
    # iter not supported in host currently
    return list(iter(arg))


@native
def is_callable(e):
    return callable(e)


Env, is_env, (env_d, env_parent), _ = __defstruct('Env', 'd', 'parent')


@native
class Env:
    def __init__(self, d, parent):
        self.d = d
        self.parent = parent


@native
def is_env(o):
    return isinstance(o, Env)


@native
def env_d(e):
    return e.d


@native
def env_parent(e):
    return e.parent


# constructor with default values
def make_env(parent=None):
    return Env({}, parent)


def env_contains(env, k):
    assert is_symbol(k), k
    assert(is_env(env))
    return k in env_d(env) or (env_parent(env) and env_contains(env_parent(env), k))


def env_get(env, k):
    assert is_symbol(k), k
    assert(is_env(env))
    while env is not None:
        d = env_d(env)
        if k in d:
            return d[k]
        else:
            env = env_parent(env)
    # TODO bind
    raise KeyError(k)


def env_containing_parent(env, k):
    assert is_symbol(k), k
    assert(is_env(env))
    while env and k not in env_d(env):
        env = env_parent(env)
    return env


def env_def(env, k, v):
    assert is_symbol(k), k
    assert(is_env(env))
    d = env_d(env)
    assert not k in d, '{k} in {d}'.format(k=k, d=env_d(env))
    #print('~~~~~~~~env_def:', k, '=', sexps_str(v), sexps_str(env_d(env)))
    env_d(env)[k] = v


def env_change(env, k, v):
    assert is_symbol(k), k
    env = env_containing_parent(env, k) or env
    #print('        env_change:', k, '=', sexps_str(v), env_d(env).keys())
    env_d(env)[k] = v
# from .base import __defstruct, py_bind_env


symbols = {}

# symbol, is_symbol, (symbol_name, ), _symbol_setters = __defstruct('symbol', 'name')


@native
class symbol:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'symbol(' + self.name + ')'

@native
def is_symbol(s):
    return isinstance(s, symbol)


@native
def symbol_name(s):
    return s.name


def intern(s):
    assert(is_str(s)), s
    if s not in symbols:
        sym = symbol(s)
        symbols[s] = sym
        return sym
    else:
        return symbols[s]


gensym_counter = -1

@native
def gensym(prefix='g'):
    if is_symbol(prefix):
        prefix = symbol_name(prefix)
    global gensym_counter
    gensym_counter += 1
    return symbol('__' + prefix + '-' + str(gensym_counter))
# from .symbol import intern


whitespace = ' \t\n'
newlines = '\n' #TODO add windows mac shit
token_end_chars = whitespace + ')'

keyword_start = ':'
floating_point = '.'
str_start = '"'
str_end = '"'

accessor_char = "."
quote_char = "'"
backquote_char = "`"
# clojur style
backquote_eval_char = "~"
backquote_splice_char = "~@"
comment_chars = ";", 
escape_chars = '\\'

special_chars = dict(n='\n', t='\t')


quote_fun_name = "quote"
backquote_fun_name = "backquote"
backquote_eval_fun_name = "unquote"
backquote_splice_fun_name = "unquote-splice"

return_name = 'return'
return_sym = intern(return_name)
accessor_char_sym = intern(accessor_char)

variadic_name = '&rest'
keys_name = '&keys'
nokeys_name = '&nokeys'

nokeys_sym = intern(nokeys_name)

quote_fun_sym = intern(quote_fun_name)
backquote_fun_sym = intern(backquote_fun_name)
backquote_eval_fun_sym = intern(backquote_eval_fun_name)
backquote_splice_fun_sym = intern(backquote_splice_fun_name)
global_env_sym = intern('*global-env*')


VALID = '__VALID'
RETURN = '__RETURN'
FAILED = '__FAILED'


def Valid(expr):
    return (VALID, expr)


def Return(expr):
    return (RETURN, expr)


def Failed():
    return (FAILED, None)


VALID_ACTIONS = VALID, RETURN


def ends_token(s):
    """assumes that all tokens are ended by one of token_end_chars"""
    return stream_peek(s) in token_end_chars


def is_paren_open(c):
    return c == '('


def is_paren_close(c):
    return c == ')'


def is_whitespace(c):
    return c in " \n\t"


def Stream(source, pos):
    return dict(source=source, pos=pos)


def stream_source(self):
    return self['source']


def stream_pos(self):
    return self['pos']


def stream_pos_set(self, pos):
    self['pos'] = pos


def stream_token(self, a, b):
    return stream_source(self)[a:b]


def stream_peek(self):
    return stream_source(self)[stream_pos(self)]


def stream_next(self):
    c = stream_source(self)[stream_pos(self)]
    stream_pos_set(self, stream_pos(self) + 1)
    return c


def stream_advance(self, n):
    stream_pos_set(self, stream_pos(self) + n)


def stream_empty(self):
    return stream_pos(self) >= len(stream_source(self))


def next_token_is(reader, s):
    return reader(s) != 0


def make_parse_fail(msg):
    def parse_fail(token):
        raise Exception('unexpected %s: %s' % (token, msg))
    return parse_fail


def read_list(s):
    if stream_empty(s) or not is_paren_open(stream_next(s)):
        return Failed()
    else:
        # TODO check if ) throws error
        def read_list_end(s):
            if stream_empty(s) or not is_paren_close(stream_next(s)):
                return Failed()
            return Return(None)

        els = read(s, readers=[read_list_end] + readers)
        return Valid(els)


def _read_int(s):
    istart = stream_pos(s)
    unary = 0
    if not stream_empty(s) and stream_peek(s) in ('-', '+'):
        stream_next(s)
        unary = 1
    while not stream_empty(s) and stream_peek(s) in '0123456789':
        stream_next(s)
    return istart + unary < stream_pos(s)
        

def read_num(s):
    istart = stream_pos(s)
    parsed = _read_int(s)
    num = int
    if not stream_empty(s) and stream_peek(s) in floating_point:
        stream_next(s)
        parsed = _read_int(s)
        num = float
    parsed = parsed and (stream_empty(s) or ends_token(s))
    if parsed:
        s = stream_token(s, istart, stream_pos(s))
        return Valid(num(s))
    return Failed()


def read_str(s):
    if stream_peek(s) in str_start:
        stream_next(s)
        r = ''
        escape_open = False
        while not stream_empty(s) and (stream_peek(s) not in str_end or escape_open):
            c = stream_next(s)
            if c in escape_chars:
                if escape_open:
                    r += c
                escape_open = not escape_open
            else:
                if escape_open and c in special_chars:
                    c = special_chars[c]
                r += c
                escape_open = False
        if not stream_empty(s):
            stream_next(s)
            return Valid(r)
    return Failed()


def read_whitespace(s):
    parsed = None
    while not stream_empty(s) and stream_peek(s) in whitespace:
        stream_next(s)
        parsed = True
    return Valid(None) if parsed else Failed()


def read_comment(s):
    if stream_next(s) not in comment_chars:
        return Failed()
    while not stream_empty(s) and stream_peek(s) not in newlines:
        stream_next(s)
    return Valid(None)




def read_symbol(s):
    istart = stream_pos(s)
    while not (stream_empty(s) or ends_token(s)):
        stream_next(s)
    if stream_pos(s) != istart:
        token = stream_token(s, istart, stream_pos(s))
        def resp():
            return Valid(intern(token))
        if accessor_char not in token:
            return resp()
        else:
            if token == accessor_char:
                return resp()
            accessors = token.split(accessor_char)
            # we have something like ..foo
            if any([a == '' for a in accessors]):
                return resp()
            accessors = [a for a in accessors if a]
            
            return Valid([accessor_char_sym] + [intern(s) for s in accessors])
    return Failed()
        

def read_quote_like(s, quote_char, quote_call_sym):
    for c in quote_char:
        if stream_next(s) != c:
            return Failed()
    # quote only supports one following exp
    expr =  read(s, one=True)
    
    r = [quote_call_sym] + expr
    return Valid(r)


def read_quote(s):
    return read_quote_like(s, quote_char, quote_fun_sym)
    

def read_backquote(s):
    return read_quote_like(s, backquote_char, backquote_fun_sym)
    

def read_backquote_eval(s):
    return read_quote_like(s, backquote_eval_char, backquote_eval_fun_sym)
    

def read_backquote_splice(s):
    return read_quote_like(s, backquote_splice_char, backquote_splice_fun_sym)
    

readers = [
    (read_list)
    , (read_whitespace)
    , (read_comment)
    , (read_num)
    , (read_str)
    , (read_backquote_splice)
    , (read_backquote_eval)
    , (read_quote)
    , (read_backquote)
    , (read_symbol)
]


def read(s, readers=readers, one=False):
    r = []
    action = FAILED
    while not stream_empty(s) and not action is RETURN:
        for reader in readers:
            istart = stream_pos(s)
            action, res = reader(s)
            if action in VALID_ACTIONS:
                e = res
                if e is not None:
                    r.append(e)
                break
            else:
                stream_pos_set(s, istart)
        if not action in VALID_ACTIONS:
            raise Exception('Unexpected: "%s" at %s' % (stream_peek(s), stream_pos(s)))
        # one reads until one actual token is parsed
        elif r and one:
            break
    return r

# from .base import native, __defstruct, is_struct, TYPE
# from .base import keyword, keyword_name, is_keyword, special_form, is_special_form, is_list, is_num, is_str, is_int, is_atom, is_callable, length, special_form_get_fun, is_special_keyword, Macro, is_macro, macro_get_fun
# from .base import make_env, env_contains, env_get, env_def, env_change, env_d, env_parent
# from .base import sexps_str, ps, is_named_operator
# from .symbol import intern, symbol_name, is_symbol, gensym
# from .reader import read, Stream, quote_fun_name, backquote_fun_name, backquote_eval_fun_name, backquote_splice_fun_name, quote_char, backquote_char, backquote_eval_char, backquote_splice_char, keyword_start
import operator

import inspect

@native
class BlockException(Exception):
    def __init__(self, name, value=None):
        Exception.__init__(self, name)
        self.name = name
        self.value = value


def format_operator_call(fun, args):
    args = [sexps_str(a) for a in args]
    return '({fun} {args})'.format(fun=fun, args=' '.join(args))


callstack = []


def callstack_str():
    max_n = 50
    long = len(callstack) > max_n
    partial_callstack = reversed(list(reversed(callstack))[:max_n]) if long else callstack
    indent = '### ' * (1 + get_interpreter_meta_level())
    def stack_line(f, args):
        return indent + format_operator_call(sexps_str(f), args)
    stack_strs = [stack_line(*line) for line in partial_callstack]
    msg = None
    if long:
        msg = '<truncated {num} entries>'.format(num=len(callstack) - max_n)
    else:
        msg = '<beginning>'
    r = '\n'.join([indent + msg] + stack_strs + [indent + '<end>'])
    return r

def make_error_msg(msg, **kwargs):
    s = '''Traceback (most recent call last):
{stack}
{msg}'''

    return s.format(stack=callstack_str(), msg=msg.format(**kwargs))
    

def defstruct(env, name, *fields):
    assert(is_symbol(name)), 'defstruct: {s}'.format(s=name)
    name_str = symbol_name(name)
    field_names = [symbol_name(f) for f in fields]

    constructor, is_instance, getter, setter = __defstruct(name_str, *field_names)
    env_def(env, name, constructor)
    fname = '%s?' % (name_str)
    env_def(env, intern(fname), is_instance)
    for field, get in zip(field_names, getter):
        gname = '%s-%s' % (name_str, (field))
        env_def(env, intern(gname), get)

    for field, set in zip(field_names, setter):
        sname = '%s-%s-set' % (name_str, (field))
        env_def(env, intern(sname), set)

    return constructor


functions = {}


def add_function(f, *args):
    f = id(f)
    functions[f] = args
    assert f in functions, functions


def patch_function_name(f, name):
    assert(is_str(name))
    f = id(f)
    assert(f in functions), '{f}\n{fs}'.format(fs=functions, f=f)
    args = functions[f]
    data = name, args[1], args[2], args[3], args[4]
    functions[f] = data
    debug('patching ', name)


def function_name(f):
    f = id(f)
    return functions[f][0] if f in functions else str(f)


def function_info_nokeys(info):
    return info[2]


@native
def block(env, name, *body):
    assert is_symbol(name), 'block: {s}'.format(s=name)
    try:
        return __progn(env, *body)
    except BlockException as e:
        assert is_symbol(e.name), e.name
        if e.name != name:
            raise e
        return e.value


@native
def return_from(env, name, value=None):
    assert (is_symbol(name)), 'return_from: {s}'.format(s=name)
    r = __eval(env, value) if value is not None else None
    raise BlockException(name, r)
    

def parameter_default(p):
    return p[1]


def is_parameter_with_default_(p):
    return is_list(p) and len(p) == 2 and is_symbol(p[0])


def is_simple_parameter(p):
    return is_symbol(p) and not is_keyword(p) and not is_special_keyword(p)


def is_normal_parameter(p):
    return is_simple_parameter(p) or is_parameter_with_default_(p)


def normal_parameter_name(p):
    if is_parameter_with_default_(p):
        return p[0]
    else:
        return p

function, is_function, (function_env, function_parameters, function_varargs_name, function_keysargs_name, function_block_name, function_body), _ = __defstruct('function', 'env', 'parameters', 'varargs_name', 'keysargs_name', 'block_name', 'body')


def __fn(env, parameters, *body):

    specials = {variadic_name, keys_name, nokeys_name}
    special_allows_next = {variadic_name, keys_name}
    # TODO support start_only specials
    special_end_only = {variadic_name, keys_name}
    special_once_only = {variadic_name, keys_name, nokeys_name}

    parsed_parameters = []
    special_used = set()

    special_params = {}
    special_params[variadic_name] = None
    special_params[keys_name] = None

    varargs_param = False
    kwargs_param = False

    used_names = set()
    i = 0
    end = False
    while i < len(parameters):
        iparam = i
        param = parameters[i]
        param_name = None
        param_default = None
        param_special = None

        is_last = i + 1 >= len(parameters)
        next_param = parameters[i + 1] if not is_last else None
        has_normal_next = not is_last and next_param and is_normal_parameter(next_param)
        if is_special_keyword(param):
            param_special = symbol_name(param)
            param = None
            if param_special not in specials:
                raise Exception(make_error_msg('Unkown special keyword: {s} at position {i}', s=p, i=i))
            if param_special in special_params:
                special_params[param_special] = True
            if param_special in special_allows_next:
                if has_normal_next:
                    # skip next parameter which we just parsed
                    i += 1
                    if param_special in special_params:
                        special_params[param_special] = next_param
                
            if param_special in special_once_only and param_special in special_used:
                raise Exception(make_error_msg('{special} parameter defined more than once', special=param_special))
            if param_special in special_end_only:
                end = True
            elif end:
                raise Exception(make_error_msg('{special} parameters must be defined after normal ones', special=param_special))

            if param_special is not None:
                special_used.add(param_special)

        if param:
            param_name = normal_parameter_name(param)
            if is_parameter_with_default_(param):
                param_default = parameter_default(param)
                def make_default_constructor(param_default):
                    def constructor():
                        return __eval(env, param_default)
                    return constructor
                param_default = make_default_constructor(param_default)
        if param_name:
            if symbol_name(param_name) in used_names:
                raise Exception(make_error_msg('Duplicate parameter {name}', name=n))
            used_names.add(symbol_name(param_name))
            assert is_symbol(param_name), param_name
            parsed_parameters.append((param_name, param_default))
        i += 1

    block_name = gensym('fn')

    varargs_name = special_params[variadic_name]
    keysargs_name = special_params[keys_name]

    user_function = function(env, parsed_parameters, varargs_name, keysargs_name, block_name, body)

    #print('&&&&&&&&', special_used, sexps_str(parameters), nokeys_name in special_used)
    add_function(user_function
                 , None # name
                 , parsed_parameters
                 , nokeys_name in special_used
                 , variadic_name in special_used
                 , keys_name in special_used
    )
    #assert is_callable(user_function), repr(user_function)
    return user_function


def __defun(env, name, parameters, *body):
    assert is_symbol(name), 'defun: {s}'.format(s=name)
    #if env_contains(env, name):
    #    raise Exception(make_error_msg('fun {fun} already declared', fun=symbol_name(name)))

    name_str = symbol_name(name)
    f = __fn(env, parameters, *body)
    patch_function_name(f, name_str)
    env_def(env, name, f)
    return f


def __defmacro(lexical_env, name, parameters, *body):
    assert(is_symbol(name)), '{i}: {call}'.format(
        i=get_interpreter_meta_level()
        , call=format_operator_call('__defmacro', [lexical_env, name, parameters] + list(body)))
    if env_contains(lexical_env, name):
        raise Exception(make_error_msg('fun {fun} already declared', fun=symbol_name(name)))

    f = __fn(lexical_env, parameters, *body)
    m = macro(f)
    env_def(lexical_env, name, m)
    return m


def __apply(env, f_form, args):
    f = __eval(env, f_form)
    callstack.append((function_name(f), [args]))
    evaled_args = __eval(env, args)
    r = __call(env, f, evaled_args, do_eval_args=False)
    callstack.pop()
    return r
    

def __sub_env(env, *body):
    sub_env = make_env(env)
    return __progn(sub_env, *body)


@native
def __let(env, vars, *let_body):
    for var in vars:
        assert(is_list(var)), sexps_str(var)
        assert(len(var) == 2)
        assert (is_symbol(var[0])), sexps_str(var)

    let_env = make_env(env)

    # TODO support double assignment to same var
    for var in vars:
        name_sym, body = var
        val = __eval(let_env, body)
        # TODO we should have something like env_set
        env_d(let_env)[name_sym] = val
    return __progn(let_env, *let_body)


@native
def __if(env, condition, then, *else_body):
    r = None
    if __eval(env, condition):
        r = __eval(env, then)
    elif else_body:
        r = __progn(env, *else_body)
    return r


def __def(env, name, *args):
    val = __eval(env, args[0]) if args else None

    assert(is_symbol(name)), 'def: {s}'.format(s=name)
    env = env_get(env, global_env_sym)
    if env_contains(env, name):
        raise Exception(make_error_msg('var {var} already declared in global env', var=symbol_name(name)))
    env_def(env, name, val)
    return val


def __setq(env, name, value):
    assert(env is not None)
    assert(is_symbol(name)), 'set: {s}'.format(s=name)
    #if not env_contains(env, name):
    #    raise Exception(make_error_msg('set: {sym} not declared in {env} ({is_env})'
    #                    , sym=symbol_name(name), env=sexps_str(env_d(env)), is_env=sexps_str(env_d(env_parent(env)) if env_parent(env) else '{}'))
    value = __eval(env, value)
    if is_callable(value) and not is_native_builtin(value) and id(value) in functions and not functions[id(value)][0]:
        patch_function_name(value, symbol_name(name))
    env_change(env, name, value)
    return value


@native
def py_get_param_names(obj):
    sig = inspect.Signature.from_callable(obj)
    P = inspect.Parameter

    args = []
    varargs = False
    varkwargs = False
    for p in sig.parameters.values():
        if p.kind == P.VAR_POSITIONAL:
            varargs = p.name
        elif p.kind == P.VAR_KEYWORD:
            varkwargs = p.name
        else:
            args.append(p.name)
    return args, varargs, varkwargs, False


native_functions = dict()


@native
def native_set_nokeys(fun, nokeys):
    fun = get_native_function_id(fun)
    #parameters = py_get_param_names(fun)
    native_functions[fun] = nokeys


@native
def is_native_function(fun):
    return type(fun).__name__ in ('function', 'builtin_function_or_method')


@native
def is_native_builtin(fun):
    # TODO: ugly hack around: [].append in {} => unhashable type
    return type(fun).__name__ == 'builtin_function_or_method'


@native
def get_native_function_id(fun):
    if is_native_builtin(fun):
        c = fun.__self__.__class__
        if c in (list, dict):
            fun = fun.__self__.__class__.__name__ + '_' + fun.__name__
    else:
        fun = id(fun)
    return fun

@native
def get_native_function_info(fun):
    fun = get_native_function_id(fun)
    if fun not in native_functions:
        #parameters = py_get_param_names(fun)
        native_set_nokeys(fun, False)
        return False
    else:
        return native_functions[fun]


@native
def get_host_function_info(fun):
    return functions.get(fun, None)


def get_function_info(fun):
    return functions.get(id(fun), None)


def __call_function(env, fun, args_forms, eval):
    nokeys = False

    first_arg = args_forms[0] if args_forms else None
    if first_arg == nokeys_sym:
        nokeys = True
        args_forms.pop(0)

    args_ = args_forms
    # apply might already have evaled arguments
    if eval:
        args_ = [__eval(env, arg) for arg in args_]

    function_info = None
    is_native = is_native_builtin(fun)
    if not is_native:
        function_info = get_function_info(fun)
        if function_info is None:
            function_info = get_host_function_info(fun)
        debug(lambda: repr(fun), function_info)
        if function_info:
            nokeys = nokeys or function_info_nokeys(function_info)

    # native functions
    if is_native or function_info is None:
        nokeys = get_native_function_info(fun)


    iparam = 0
    args = []
    kwargs = {}

    remaining_args = args_
    keywords_started = False
    while remaining_args:
        arg = remaining_args[0]
        remaining_args = remaining_args[1:]

        if is_keyword(arg) and remaining_args and not nokeys:
            keywords_started = True
            key = keyword_name(arg)
            arg = remaining_args.pop(0)
            kwargs[key] = arg
        else:
            if keywords_started:
                raise Exception(make_error_msg('positional argument follows keyword argument'))

            args.append(arg)
        del arg


    if is_native or function_info is None:
        def call_function():
            return fun(*args, **kwargs)

        @native
        def call_function():
            try:
                return fun(*args, **kwargs)
            except TypeError as e:
                raise Exception(make_error_msg('{e} from {call} with kwargs: {kwargs}', e=e, call=format_operator_call(fun, args), kwargs=kwargs))

        return call_function()
    else:
        # self-defined fun
        (function_name, parameters, nokeys_def, set_varargs, set_kwargs) = function_info

        function_repr = function_name or 'fn'

        if not set_varargs and len(args) > len(parameters):
            raise Exception(make_error_msg('''too many arguments (#{n} vs #{m}) in function call:
    {call}
parsed as:
    {parsed}
called with:
    {args} &rest {varargs} &keys {kwargs}
parameters:
    {params}.'''
                                           , call=format_operator_call(function_repr, args_forms)
                                           , parsed=format_operator_call(function_repr, args_)
                                           , args=format_operator_call(function_repr, args)
                                           , kwargs=kwargs
                                           , params=sexps_str(parameters)
                                           , varargs=repr(set_varargs)
                                           , n=len(parameters)
                                           , m=len(args)
            ))

        # try defaults, extract missing positional args from kwargs
        if len(args) < len(parameters):
            for i, p in enumerate(parameters[len(args):]):
                assert(isinstance(p, tuple)), p
                (param_name, param_default) = p
                n = symbol_name(param_name)
                in_kwargs = n in kwargs
                if not in_kwargs and param_default is None:
                    raise Exception(make_error_msg('function call missing argument "{name}" {default}: {call} -- forms: {forms}'
                                                   , name=n, default=param_default() if param_default else '', call=format_operator_call(function_repr, args), forms=sexps_str(args_forms)))
                args.append(kwargs[n] if in_kwargs else param_default())
                if in_kwargs:
                    del kwargs[n]

        # even afer adding default values still not enough args
        if len(parameters) > len(args):
            raise Exception(make_error_msg('function call missing argument "{i}": {call}'
                                           , i=symbol_name(parameters[len(args)][0]), fun=format_operator_call(function_repr, args_forms)))

        if not set_kwargs and kwargs:
            raise Exception(make_error_msg('Unexpected keyword arguments for function call: ({fun} {params} {kwargs}) called with {args}'
                                           , fun=function_repr, args=sexps_str(args_forms), params=sexps_str(parameters), kwargs=sexps_str(kwargs)))

        varargs = []
        if set_varargs and len(args) > len(parameters):
            varargs = args[len(parameters):]
            args = args[:len(parameters)]

        def user_function(fun, args, varargs, kwargs):
            fun_env = make_env(env)

            block_name = function_block_name(fun)
            return_fun = special_form(lambda call_env, value=None: return_from(call_env, block_name, value))
            env_def(fun_env, return_sym, return_fun)

            for (parameter, default), arg in zip(function_parameters(fun), args):
                env_def(fun_env, parameter, arg)

            varargs_name = function_varargs_name(fun)
            if is_symbol(varargs_name):
                env_def(fun_env, varargs_name, varargs)
            keysargs_name = function_keysargs_name(fun)
            if is_symbol(keysargs_name):
                env_def(fun_env, keysargs_name, kwargs)

            body = function_body(fun)
            return block(fun_env, block_name, *body)
            
        return user_function(fun, args, varargs, kwargs)
    

def __call(env, fun, args_forms, do_eval_args):
    if is_special_form(fun):
        fun = special_form_fun(fun)
        debug(lambda: repr(fun))
        r = __call_function(env, fun, [env] + args_forms, eval=False)
        return r
    elif is_macro(fun):
        fun = macro_fun(fun)
        form = __call_function(env, fun, args_forms, eval=False)
        return __eval(env, form)

    elif is_function(fun) or is_callable(fun):
        return __call_function(env, fun, args_forms, do_eval_args)

    else:
        raise Exception(make_error_msg('({fun} {args}) is not callable', fun=repr(fun), args=sexps_str(args_forms) if args_forms else ''))


def __eval(env, form):
    debug('******** eval :', lambda: sexps_str(form))
    r = None
    if is_symbol(form) and not is_keyword(form):
        if not env_contains(env, form):
            def print_env_keys(env):
                r = []
                while env is not None:
                    r += ['Keys: ' + ' '.join(sorted(map(symbol_name, as_list(env_d(env).keys()))))]
                    env = env_parent(env)
                return '\n'.join(r)
                    

            raise Exception(make_error_msg('Symbol "{sym}" not found in env \n{keys}'
                                           , sym=symbol_name(form)
                                           , keys=print_env_keys(env)))
        r = env_get(env, form)
    elif is_atom(form):
        r = form
    elif is_list(form) and form:
        debug(lambda: sexps_str(form))
        args_forms = form[1:]
        callstack.append((form[0], args_forms))
        fun = __eval(env, form[0])
        assert is_macro(fun) or is_special_form(fun) or is_function(fun) or is_callable(fun), fun
        r = __call(env, fun, args_forms, do_eval_args=True)
        callstack.pop()
    else:
        raise Exception(make_error_msg('unknown form: {form}', form=sexps_str(form)))
    debug('******** eval returning:', lambda: repr(form))
    return r
        

def base_env(args=[]):
    debug('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CREATING BASEENV for interpreter', get_interpreter_meta_level())
    env = make_env()

    env_def(env, intern('__interpreter_meta_level'), get_interpreter_meta_level() + 1)

    def bind(name, value):
        assert is_str(name), name
        env_def(env, intern(name), value)

    def bindn(*args):
        assert len(args) >= 2, args
        names = args[:-1]
        value = args[-1]
        for name in names:
            bind(name, value)


    # value binds
    bind('true', True)
    bind('false', False)
    bind('nil', None)
            
    # binds that don't need to be wrapped
    bind('type', type)
    # FIXME implement in stdlib
    bind('sorted', sorted)

    # list basics
    bindn('as-list', 'as_list', as_list)

    @native
    def list_set(l, k, v):
        assert(is_list(l))
        assert(is_int(k))
        l[k] = v
        return v
    bindn('list-set', 'list_set', list_set)

    @native
    def append(l, *es):
        for e in es:
            l.append(e)
        return l
    bind('append', append)

    @native
    def extend(l, *ls):
        l = list(l)
        for e in ls:
            l += e
        return l
    bind('extend', extend)

    bind('aref', lambda l, k: l[k])

    # needs to be wrapped
    bind('intern', intern)
    bind('symbol', symbol)
    bindn('symbol-name', 'symbol_name', symbol_name)

    bind('keyword', keyword)
    bind('keyword-name', keyword_name)


    @native
    def dict_set(d, k, v):
        d[k] = v
    bindn('dict-set', 'dict_set', dict_set)

    @native
    def dict_keys(d):
        return as_list(d.keys())
    bindn('dict-keys', 'dict_keys', dict_keys)


    env_def(env, 'callable?', is_callable)
    env_def(env, 'is_callable', is_callable)

    @native
    def __while(env, condition, *body):
        while __eval(env, condition):
            __progn(env, *body)
            #__eval(env, [intern('progn')] + list(body))


    # assert is not a function thus pain
    @native
    def __assert(env, condition, msg=''):
        r = __eval(env, condition)
        if not r:
            msg = __eval(env, msg)
            msg = '%s: %s' % (sexps_str(condition), msg)

        assert r, msg
    bindn('assert', '__assert', special_form(__assert))
        
    bind('__block', special_form(block))
    bindn('__if', 'if', special_form(__if))
    bind('__while', special_form(__while))
    return_from_special = special_form(return_from)
    bind('return_from', return_from_special)
    bind('return-from', return_from_special)
    bind('let*', special_form(__let))
    
    # these are just for bootstrapping -- functions do not need to exist other than for python reasons
    def native_binds():
        # could use list + globals here, but this is easier to bootstrap

        bind('list', list)
        bind('tuple', tuple)
        bind('dict', dict)

        bind('dict_setdefault', dict_setdefault)
        bind('dict-setdefault', dict_setdefault)


    @native
    def native_binds():
        def __list(*args):
            return list(args)
        bind('list', __list)

        def __tuple(*args):
            # tuple doesn't take more than one arg
            return tuple(args)
        bind('tuple', __tuple)

        def __dict(**kwargs):
            return dict(**kwargs)
        bind('dict', __dict)

        bindn('repr', 'sexps_str', sexps_str)

        bind('dict_setdefault', dict.setdefault)
        bind('dict-setdefault', dict.setdefault)
    
        
    native_binds()
    def set_nokeys_from_env(name):
        f = env_get(env, name)
        native_set_nokeys(f, True)
    set_nokeys_from_env(intern('list'))
    set_nokeys_from_env(intern('tuple'))

    native_set_nokeys(__defmacro, True)


    
    def __import(env, *args):
        # TODO 
        pass
    bind('import', special_form(__import))

    @native
    def py_import(env, *args):
        import importlib
        for module in args:
            if is_symbol(module):
                module_name = symbol_name(module)
                module = importlib.import_module(module_name)
                env_def(env, intern(module_name), module)
            elif is_list(module):
                first = module[0]
                assert(is_symbol(first)), sexps_str(module)
                first_name = symbol_name(first)
                level = 0
                if all([c == '.' for c in first_name]):
                    level = len(first_name)
                    assert(len(module) > 1), sexps_str(module)
                    module = module[1:]
                    assert(False), 'relative imports from lisp not allowed'
                if len(module) > 1:
                    froms = module[1:]
                    module = module[0]

                assert(is_symbol(module))
                assert(is_list(froms))
                for f in froms:
                    # TODO support (:alias name)
                    assert(is_symbol(f))

                module_name = symbol_name(module)
                module = importlib.__import__(module_name, fromlist=fromlist)
                if froms:
                    fromlist = [symbol_name(f) for f in froms]
                    for from_name in fromlist:
                        env_def(env, intern(from_name), getattr(module, from_name))
                else:
                    env_def(env, intern(module_name), module)
            else:
                assert(False), sexps_str(module)
        pass
    bind('py-import', special_form(py_import))
    bind('py_import', py_import)

    @native
    def __lookup(env, obj, *ks):
        r = __eval(env, obj)
        for k in ks:
            assert(is_symbol(k)), sexps_str(k)
            r = getattr(r, symbol_name(k))
        return r
    bind('.', special_form(__lookup))
    bind('__lookup', __lookup)

    bind(quote_fun_name, special_form(lambda env, e: e))

    backquote_level_var = '*__backquote_level*'

    def backquote_(env, s):
        if is_atom(s):
            return [s]
        elif is_named_operator(s, backquote_eval_fun_sym):
            arg = s[1]
            assert(len(s) == 2)
            #print('~:', level, sexps_str(s))

            form = s[1]
            nested_level = 0
            while is_named_operator(form, backquote_eval_fun_sym):
                nested_level += 1
                form = form[1]
            if is_named_operator(form, backquote_splice_fun_sym):
                nested_level += 1
            # sanity check
            #assert(nested_level <= level), '%s is deeper than %s in %s' % (nested_level, level, sexps_str(s))

            # use first level again
            form = s[1]
            # only eval top level, otherwise throw away one level
            r = form
            if nested_level == 0:
                r = __eval(env, form)
            #print('~:', level, sexps_str(form))
            return [r]
        elif is_named_operator(s, backquote_splice_fun_sym):
            #print('~@:', level, sexps_str(s))
            assert(len(s) == 2)
            form = s[1]
            r = __eval(env, form)
            #print('~@:', level, sexps_str(r))
            assert(is_list(r)), (r, sexps_str(s))
            return r
        elif is_named_operator(s, backquote_fun_sym):
            #print('`:', level, sexps_str(s))
            assert(len(s) == 2)
            form = s[1]
            r = backquote_(env, form)
            assert(len(r) == 1)
            r = [[backquote_fun_sym] + r]
            #print('`:', level, sexps_str(r))
            return r
        elif is_list(s):
            r = []
            for e in s:
                #print('----1', sexps_str(e))
                e = backquote_(env, e)
                #print('----2', sexps_str(e))
                r += e
            return [r]
        else:
            raise Exception(make_error_msg(sexps_str(s)))

    def backquote(env, s):
        #print('backquote:', level, sexps_str(s))
        r = backquote_(env, s)
        assert(len(r) == 1)
        r = r[0]
        #print('backquote:', level, sexps_str(r))
        return r

    bind(backquote_fun_name, special_form(backquote))

    def source_eval(env, form):
        form = __eval(env, form)
        return __eval(env, form)
    bind('eval', special_form(source_eval))
    bind('set', special_form(__setq))
    bind('__sub-env', special_form(__sub_env))
    bind('progn', special_form(__progn))

    bind('def', special_form(__def))
    bind('defun', special_form(__defun))
    bind('defmacro', special_form(__defmacro))
    bind('fn', special_form(__fn))
    bind('apply', special_form(__apply))

    bindn('callable?', 'is_callable', is_callable)

    bind('gensym', special_form(lambda env, *args: gensym(*args)))

    bind('null?', lambda *args: all([e is None for e in args]))

    tests = dict(
        symbol=is_symbol
        , keyword=is_keyword
        , list=is_list
        , tuple=is_tuple
        , dict=is_dict
        , num=is_num
        , float=is_float
        , int=is_int
        , str=is_str
        , atom=is_atom
    )
    for k, f in list(tests.items()):
        bind('{s}?'.format(s=k), f)
        bind('is_{s}'.format(s=k), f)

    bind('named-operator?', is_named_operator)

    def numeric_op(op):
        def numeric_op(a, *args):
            r = a
            for b in args:
                r = op(r, b)
            return r
        return numeric_op

    bind('+', numeric_op(operator.__add__))
    bind('-', numeric_op(operator.__sub__))
    bind('*', numeric_op(operator.__mul__))
    bind('/', numeric_op(operator.__truediv__))
    bind('mod', numeric_op(operator.__mod__))

    bind('eq', operator.__eq__)
    bind('neq', operator.__ne__)

    @native
    def __is(a, *bs):
        return all([a is b for b in bs])
    bind('__is', __is)

    bind('not', operator.__not__)

    def _and(env, *tests):
        r = None
        for test in tests:
            r = __eval(env, test)
            if not r:
                return None
        return r
    bind('and', special_form(_and))
    def _or(env, *tests):
        r = None
        for test in tests:
            r = __eval(env, test)
            if r:
                return r
        return None
    bind('or', special_form(_or))

    bind('<', operator.__lt__)
    bind('<=', operator.__le__)
    bind('>', operator.__gt__)
    bind('>=', operator.__ge__)

    def cons(e, l):
        assert is_list(l), repr(l)
        return [e] + l
    bind('cons', cons)

    @native
    def slice(l, istart=None, *args):
        n = len(args)
        assert(n <= 2)
        if n < 2:
            iend = None
            if n == 0:
                iend = istart
                istart = 0
            else:
                iend = args[0]

            if istart is None:
                istart = 0
            if iend is None:
                iend = len(l)

            if istart > iend:
                iend += len(l)
            if istart < 0:
                assert iend <= 0, iend
                istart += len(l)
                iend += len(l)

            assert istart >= 0
            assert iend >= 0
            return l[istart:iend]
        else:
            if istart is None:
                istart = 0
            iend, step = args
            if iend is None:
                iend = len(l)
            if step is None:
                step = 1
            r = [l[i] for i in range(istart, iend, step)]
            if is_str(l):
                return ''.join(r)
            else:
                return r
    bind('slice', slice)


    bind('length', length)

    @native
    def contains(l, e):
        assert is_list(l) or is_tuple(l) or is_dict(l)
        return e in l
    bindn('contains?', 'contains', contains)

    def nth(i, l):
        assert (is_list(l) or is_tuple(l) or is_str(l)),  'nth: {i} {l} ({t})'.format(i=sexps_str(i), l=sexps_str(l), t=type(l))
        return l[i]
    bind('nth', nth)

    def tail(l):
        return l[1:]
    bind('tail', tail)
    
    bind('defstruct', special_form(defstruct))
    bind('__defstruct', __defstruct)


    def throw(e):
        raise e
    bind('throw', throw)

    def exception(s):
        return Exception(s)
    bind('Exception', exception)
    
    # sys utils
    def file_open(filename, mode):
        if is_symbol(mode):
            mode = symbol_name(mode)
        assert(is_str(mode))
        return open(filename, mode)

    bind('file-open', file_open)
    
    bind('argv', args)

    import pathlib
    bind('make-Path', pathlib.Path)

    def print_(*args):
        print(*args)
    bind('print', print_)

    def princ(arg):
        ps(arg)
    bind('princ', princ)

    # TODO
    #(infix

    bind('get_native_function_info', get_native_function_info)
    bind('get_host_function_info', get_host_function_info)
    bind('is_native_builtin', is_native_builtin)
    bind('native_set_nokeys', native_set_nokeys)

    ## python interop

    bind('int', int)
    bind('float', float)
    bind('str', str)

    def py_with(env, _with, *body):
        assert is_list(_with)
        assert len(_with) == 1
        _with = _with[0]
        var = None
        if is_list(_with):
            assert len(_with) == 2
            var, _with = _with
            assert(is_symbol(var))
        with __eval(env, _with) as f:
            if var is not None:
                env_def(env, var, f)
            __progn(env, *body)
    bind('py-with', special_form(py_with))

    bind('read', read)

    env = make_env(env)
    env_def(env, global_env_sym, env)
    with open('stdlib.lisp', 'r') as f:
        interpret(read(Stream(f.read(), 0)), env)
    env = make_env(env)

    env_def(env, global_env_sym, env)
    return env


def __progn(env, *forms):
    r = None
    for form in forms:
        r = __eval(env, form)
    return r


def _interpret(forms, env=None, args=[]):
    if env is None:
        env = base_env(args)
    assert env, env
    env_def(env, intern('__name__'), '<self>')
    return __progn(env, *forms)


native_interpret = _interpret
@native
def native_interpret(*args, **kwargs):
    try:
        return _interpret(*args, **kwargs)
    except Exception as e:
        raise Exception(make_error_msg('{E}: {e}', E=type(e).__name__, e=str(e)))
    except KeyboardInterrupt as e:
        raise Exception(make_error_msg('{E}: {e}', E=type(e).__name__, e=str(e)))

interpret = _interpret
#interpret = native_interpret

# from .base import TYPE, TYPE_T
# from .symbol import intern
# from .reader import read, Stream, quote_fun_name, backquote_fun_name, backquote_eval_fun_name, backquote_splice_fun_name
# from .interpreter import interpret, ps

import unittest
import argparse

reader_tests = [
    ('''''', [])
    , ('1', [1])
    , ('1.0', [1.0])
    , ('''()''', [[]])
    , ('''(1)''', [[1]])
    , ('''(1 2)''', [[1, 2]])
    , ('''(foo)''', [[intern('foo')]])
    , ('''(foo bar)''', [[intern('foo'), intern('bar')]])
    , ('''(1+)''', [[intern('1+')]])
    , ('''(())''', [[[]]])
    , ('''((list a b ()))''', [[[intern('list'), intern('a'), intern('b'), []]]])
    , ("'a", [[intern(quote_fun_name), intern('a')]])
    , ("'()", [[intern(quote_fun_name), []]])
    , ("()1", [[], 1])
    , ("'()1", [[intern(quote_fun_name), []], 1])
    , ("`()", [[intern(backquote_fun_name), []]])
    , ("`~foo", [[intern(backquote_fun_name), [intern(backquote_eval_fun_name), intern('foo')]]])
    , ("`(~foo)", [[intern(backquote_fun_name), [[intern(backquote_eval_fun_name), intern('foo')]]]])
    , ("`(bar ~foo)", [[intern(backquote_fun_name), [intern('bar'), [intern(backquote_eval_fun_name), intern('foo')]]]])
    , ("`(bar ~foo baz)", [[intern(backquote_fun_name), [intern('bar'), [intern(backquote_eval_fun_name), intern('foo')], intern('baz')]]])
    , ("`(bar ~@ foo baz)", [[intern(backquote_fun_name), [intern('bar'), [intern(backquote_splice_fun_name), intern('foo')], intern('baz')]]])
    , ("a.b", [[intern('.'), intern('a'), intern('b')]])
    , (";a", [])
    , ("""(;a
)""", [[]])
    , ("'() ;1", [[intern(quote_fun_name), []]])
    , ("(defun foo () (+ 1 2)) (foo)", [[intern('defun'), intern('foo'), [], [intern('+'), 1, 2]], [intern('foo')]])
    , ("""(set p ((. a b)))""", [[intern('set'), intern('p'), [[intern('.'), intern('a'), intern('b')]]]])
]

interpreter_tests = [
    ('''''', None)
    # TODO check if ending ) is checked
    , ('1', 1)
    , ('1.0', 1.0)
    , ('nil', None)
    , ('true', True)
    , ('false', False)
    , ('''(list)''', [])
    , ('''(list 1)''', [1])
    , ('''(quote ())''', [])
    , ("'1", 1)
    , ("'()", [])
    , ("`1", 1)
    , ("`\"test\"", 'test')
    , ("`foo", intern('foo'))
    , ("`1", 1)
    , ("`()", [])
    , ("`(foo)", [intern('foo')])
    , ("(let* ((foo 3)) `(~foo))", [3])
    , ("(let* ((foo 3)) `(bar ~foo))", [intern('bar'), 3])
    , ("(let* ((foo 3)) `(bar ~@(list foo 1)))", [intern('bar'), 3, 1])
    , ("(destructuring-bind-parse 'target 0", [[intern('target'), 0]])
    , ("(destructuring-bind-parse '(targeta targetb) 'value)", [[intern('targeta'), [intern('nth'), 0, intern('value')]]
                                                                , [intern('targetb'), [intern('nth'), 1, intern('value')]]])
    , ("(destructuring-bind-parse '(targeta (targetb targetc)) 'value)"
       , [[intern('targeta'), [intern('nth'), 0, intern('value')]]
          , [intern('targetb'), [intern('nth'), 0, [intern('nth'), 1, intern('value')]]]
          , [intern('targetc'), [intern('nth'), 1, [intern('nth'), 1, intern('value')]]]])
    , ("(destructuring-bind-parse '((targeta targetb) targetc) 'value)"
       , [[intern('targeta'), [intern('nth'), 0, [intern('nth'), 0, intern('value')]]]
          , [intern('targetb'), [intern('nth'), 1, [intern('nth'), 0, intern('value')]]]
          , [intern('targetc'), [intern('nth'), 1, intern('value')]]])
    , ("(def target) (setf target 0) target", 0)
    , ("(def targeta) (def targetb) (setf (:= targeta targetb) '(1 2)) (list targeta targetb)"
       , [1, 2])
    , ("(let* ((l (list 1))) (setf (aref l 0) 0) l)", [0])
    , ("(let* ((l (dict :foo 1))) (setf (aref l :foo) 0) l)", dict(foo=0))
    , ("(let ((foo 3)) foo)", 3)
    , ("(let (((:= foo bar) (list 0 1))) (list foo bar))", [0, 1])
    # expected error
    #, ("(let (((tuple foo bar baz) (list 0 1))) (list foo bar))", [0, 1])
    # expected error
    #, ("(let (((tuple foo) (list 0 1))) foo)", 0)
    # expected error
    #, ("(let ((foo 3)) `(bar ,(+ ,foo 1)))", [intern('bar'), [intern('+'), 3, 1]])
    , ("(let ((foo 3)) `(bar (+ ~foo 1)))", [intern('bar'), [intern('+'), 3, 1]])
    , ("(nth 1 '(1 2 3)", 2)
    , ("(head '(1 2 3)", 1)
    , ("(tail '(1 2 3)", [2, 3])
    , ("(reversed '(1 2 3)", [3, 2, 1])
    , ("(let ((l '())) (append l 3) l)", [3])
    , ("(tuple 1 2)", (1, 2))
    , ("(tuple 'x 2)", (intern("x"), 2))
    , ("(def v (tuple 1 2))", (1, 2))
    , ("(def v (tuple 1 2))", (1, 2))
    , ('''(+ 1 2)''', 3)
    , ("(let* ((foo 3)) (+= foo 1))", 4)
    , ("(let* ((foo 3)) (+= foo 1 2))", 6)
    , ("(let* ((foo 3) (bar 2)) (+= foo 1 bar))", 6)
    , ("(let* ((foo 3)) (+= foo 1))", 4)
    , ("(let* ((foo 3)) (+= foo 1 2))", 6)
    , ("(let* ((foo 3) (bar 2)) (+= foo 1 bar))", 6)
    , ("(let* ((foo 3)) (-= foo 1))", 2)
    , ("(let* ((foo 3)) (-= foo 1 2))", 0)
    , ("(let* ((foo 3) (bar 2)) (-= foo 1 bar))", 0)
    , ("(let* ((foo 3)) (*= foo 2))", 6)
    , ("(let* ((foo 3)) (*= foo 1 2))", 6)
    , ("(let* ((foo 3) (bar 2)) (*= foo 1 bar))", 6)
    , ("(let* ((foo 6)) (/= foo 2))", 3)
    , ("(let* ((foo 6)) (/= foo 1 2))", 3)
    , ("(let* ((foo 6) (bar 2)) (/= foo 1 bar))", 3)
    , ('''(if 1 2 3)''', 2)
    , ('''(if 0 2 3)''', 3)
    , ('''(if 0 2)''', None)
    , ('''(when false 1)''', None)
    , ('''(when 1 1)''', 1)
    , ('''(def b '()) (if b b 3)''', 3)
    , ("((fn (a) (+ a 2)) 1)", 3)
    , ("(apply (fn (a) (+ a 2)) '(1))", 3)
    , ("(apply (fn (a) a) (list 'foo))", intern("foo"))
    , ("(apply (fn (a) a) (list (quote foo))", intern("foo"))
    , ("((fn (a) (+ a 2)) 1)", 3)
    , ("((fn (a &rest b) (+ a 2)) 1)", 3)
    , ("((fn (a &rest b) (+ a 2)) 1 2)", 3)
    , ("((fn (a &rest b) (if b (head b) a)) 1)", 1)
    , ("((fn (a &rest b) (if b (head b) a)) 1 2)", 2)
    , ("""
(
  (fn (a &rest b)
    (+ a (if b (head b) 2)))
  1 2)""", 3)
    , ("((fn (a) a) (+ 1 2))", 3)
    , ("((fn (a) a) (head '(3 2 1)))", 3)
    , ("((fn (a) a) (tail '(3 2 1)))", [2, 1])
    , ("(def foo 2) (set foo 1)", 1)
    , ("(def foo 1)", 1)
    , ("(def foo 1) foo", 1)
    , ("(def foo 1) (set foo 2) foo", 2)
    , ("(def foo 1) (set foo '()) foo", [])
    , ("(def foo (list 1 2)) foo", [1, 2])
    , ("(eval 1)", 1)
    , ("(eval '())", [])
    , ("(eval (+ 1 2))", 3)
    , ("(def foo) (setf foo (+ 1 2)) foo", 3)
    , ("""
(def foo0)
(def foo1)
(def foo2)
(setf (:= foo0 foo1 foo2) '(0 1 2))
(tuple foo2 foo1 foo0)
""", (2, 1, 0))
    , ("(defun foo ()) (foo)", None)
    , ("(defun foo () 1) (foo)", 1)
    , ("(defun foo () (+ 1 2)) (foo)", 3)
    , ("(defun foo (a) (+ a 2)) (foo 1)", 3)
    , ("""
(defun foo (b)
        (def a (+ 1 2))
        (* a b)
        )
        (foo 4)
""", 12)
    , ("(defun foo (a b c) (+ a (+ b c))) (foo 1 2 3)", 6)
    , ("(defun foo (a) a) (foo :a 1)", 1)
    , ("(defun foo (a) a) (foo :a :b)", intern(':b'))
    , ("(defun foo (a) a) (foo :a :a)", intern(':a'))
    , ("(defun foo (&nokeys a b) (list a b)) (foo :a 0)", [intern(':a'), 0])
    , ("(defun foo (&keys ks) ks) (foo :a 1)", dict(a=1))
    , ("(defun foo (&keys ks) ks) (foo :a 1 :b 2 :c 3)", dict(a=1, b=2, c=3))
    , ("(defun foo (a &keys ks) (tuple a ks)) (foo 1 :b 2)", (1, dict(b=2)))
    , ("(defun foo (a &keys ks) (tuple a ks)) (foo :a 1 :b 2 :c 3)", (1, dict(b=2, c=3)))
    , ("(defmacro add (a) (list '+ 5 a)) (add 1)", 6)

    , ("(defstruct Foo) (Foo)", {TYPE: {'__name__': 'Foo', 'fields': tuple()}})
    , ("(defstruct Foo a) (Foo 1)", {TYPE: {'__name__': 'Foo', 'fields':tuple('a')}, 'a': 1})
    , ("(defstruct Foo a) (def v (Foo 1)) (Foo-a v)", 1)
#    , ("(defstruct Foo a) (def v (Foo 1)) v.a", 1)
    , ("(def foo (dict :a 1))", dict(a=1))
    , ("""(let ((n 0)) (dolist (i '(0 1 2 3 4)) (set n (+ n i))) n)""", 10)
    , ("""(let ((n 3)) (dolist (i '()) (set n (+ n i))) n)""", 3)
    , ("""
(let ((n nil))
  (dolist (i '(foo bar baz))
    (print (repr i) (symbol? i))
    (when (eq i 'foo)
      (set n i)
      (print (repr n))))
    n)""", intern('foo'))
    , ("""(let ((r '())) (dolist (i '(0 1 2 3 4)) (append r i)) r)""", [0, 1, 2, 3, 4])
    , ("(foldr + 0 '(0 1 2 3 4))", 10)
    , ("""
(let ((x 5))
  (cond ((< x 3) 7)
    (true 1)))
""", 1)
    , ("""
(let ((x 5))
  (cond 
    ((< x 3) 5)
    ((< x 9) 7)
    (true 1)))
""", 7)
    , ("""
(let ((x 5))
  (cond 
    ((< x 3) 5)
    ((< x 9) 7)))
""", 7)
    , ("""(block foo (return-from foo))""", None)
    , ("""(block foo (return-from foo nil))""", None)
    , ("""(block foo (return-from foo 1))""", 1)
    , ("""(block test (return-from test nil))""", None)
    , ("""(block test (return-from test 1))""", 1)
   , ("""(block nil (break nil))""", None)
   , ("""(block nil (break 1))""", 1)
    , ("""(map (fn (e) (+ e 1)) '(0 1 2))""", [1, 2, 3])
    , ("""(list :a)""", [intern(':a')])
    , ("""(list :a 0)""", [intern(':a'), 0])
    , ("""(defun foo (a) a) (foo :a)""", intern(':a'))
    , ("""(defun foo (a b) a) (foo &nokeys :a 0)""", intern(':a'))
    , ("""(range 4)""", [0, 1, 2, 3])
    , ("""(range 0 4)""", [0, 1, 2, 3])
    , ("""(range 1 4)""", [1, 2, 3])
    , ("""(range 1 4 2)""", [1, 3])
    , ("""(range 1 5 3)""", [1, 4])
    , ("""(enumerate '(0 1 2))""", [[0, 0], [1, 1], [2, 2]])
    , ("""(enumerate '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip '(0 1 2) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip '(0 1 2 3 4) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip '(0 1 2 3) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip (range 3) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip (range 5) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip (range 3) '(foo bar baz biz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
#    , ('tuple', __tuple)
    , ('(dolist (i (range 99)) (print (repr i)))', None)
    , ('(slice (range 9) 3)', [0, 1, 2])
    , ('(slice (range 9) 3 7)', [3, 4, 5, 6])
    , ('(slice (range 9) 3 -2)', [3, 4, 5, 6])
    , ('(slice (range 9) 3 7 2)', [3, 5])
    , ('(slice (range 3) -1)', [0, 1])
    , ('(slice (range 9) -8)', [0])
    , ('(slice (range 9) -7)', [0, 1])
    , ('(slice (range 9) -3 -1)', [6, 7])
    , ('(slice (range 9) -3 0)', [6, 7, 8])
]


@native
def load_tests():
    def make_test(name, program, expected_result):
        def run(self):
            self.assertListEqual(expected_result, read(Stream(program, 0)))

        # have test name in stacktrace
        class testf(unittest.TestCase):
            pass
        setattr(testf, name, run)
        return testf(name)

    suite = unittest.TestSuite()
    for itest, (program, expected_result) in enumerate(reader_tests):
        suite.addTest(make_test('reader_%s' % itest, program, expected_result))

    def make_test(name, program, expected_result):
        def run(self):
            self.assertEqual(interpret(read(Stream(program, 0))), expected_result)

        # have test name in stacktrace
        class testf(unittest.TestCase):
            pass
        setattr(testf, name, run)
        return testf(name)

    for itest, (program, expected_result) in enumerate(interpreter_tests):
        suite.addTest(make_test('interpreter_%s' % itest, program, expected_result))
        pass

    return suite

def get_process_args():
    print('Getting process args', argv)
    return argv

@native
def get_process_args():
    import sys
    print('getting process args', sys.argv)
    return sys.argv[1:]



p = argparse.ArgumentParser()
p.add_argument('--type')
p.add_argument('--num', type=int)
p.add_argument('-c', nargs='+')
p.add_argument('args', nargs='*')

args = p.parse_args(get_process_args())

if args.type is not None and args.num is not None:
    if args.type == 'reader':
        program = reader_tests[args.num][0]
        ps(read(Stream(program, 0)))
    if args.type == 'interpreter':
        program = interpreter_tests[args.num][0]
        print('Program:', program)
        ps(interpret(read(Stream(program, 0))))
elif args.c:
    for filename in args.c:
        with open(filename, 'r') as f:
            program = f.read()
        print('Result:', interpret(read(Stream(program, 0)), args=args.args))
else:
    suite = load_tests()
    unittest.TextTestRunner().run(suite)


