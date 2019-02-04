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
    
    elif isinstance(form, list):
        r += p('[')
        for e in form:
            r += sexps_str(e, indent + 1, seen, full)
        r += p(']')
    elif is_list(form) or is_tuple(form):
        if is_seen(form):
            r += p('<cyclic list>')
        else:
            add_seen(form)

            is_simple = False
            for op, char in sexpr_print_operators.items():
                if is_list(form) and length(form) == 2 and car(form) is op:
                    assert (length(form) == 2), form
                    r += p(char + ' '.join([sexps_str(f, indent + 1, seen, full) for f in cdr(form)]).strip())
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
        print(*[a() if callable(a) else a for a in args])
        

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
                assert(is_instance(struct)), '{T}: {val}'.format(val=repr(struct), T=name_str)
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
    return is_list(form) and form and is_symbol(car(form)) and car(form) == op


special_form, is_special_form, (special_form_fun,), _ = __defstruct('special-form', 'fun')

macro, is_macro, (macro_fun,), _ = __defstruct('macro', 'fun')


def is_atom(form):
    return is_num(form) or is_str(form) or is_keyword(form) or is_symbol(form) or form is None


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


cons_end = None

class cons:
    def __init__(self, car, cdr):
        self.car = car
        assert isinstance(cdr, cons) or cdr is cons_end, cdr
        self.cdr = cdr

    def __iter__(self):
        class iter:
            def __init__(self, p):
                self.p = p

            def __iter__(self):
                return self

            def __next__(self):
                if self.p is None:
                    raise StopIteration()
                assert isinstance(self.p, cons)
                r = self.p.car
                self.p = self.p.cdr
                return r
        return iter(self)

    def __getitem__(self, i):
        while i > 0:
            return self.cdr[i - 1]
        return self.car

    def __len__(self):
        return length(self)

    def __add__(self, o):
        l = last(self)
        for e in o:
            setcdr(l, cons(e, None))
            l = cdr(l)
        return l

    def __repr__(self):
        return '(cons %s %s)' % (repr(self.car), repr(self.cdr))

    def __eq__(self, o):
        return isinstance(o, cons) and o.car == self.car and o.cdr == self.cdr

def car(c):
    return c.car


def cdr(c):
    return c.cdr


def setcar(c, v):
    c.car = v


def setcdr(c, cdr):
    assert isinstance(cdr, cons) or cdr is cons_end
    c.cdr = cdr


@native
def is_list(e):
    return isinstance(e, cons)


def last(l):
    while cdr(l) is not cons_end:
        l = cdr(l)
    return l


def nthcdr(n, l):
    for i in range(n):
        l = cdr(l)
    return l
    


@native
def cons_append(start, *ols):
    while start is cons_end:
        start = ols[0]
        ols = ols[1:]
    l = last(start)
    for ol in ols:
        assert isinstance(ol, cons) or ol is cons_end, sexps_str(ol)
        if ol is cons_end:
            continue
        setcdr(l, ol)
        l = last(l)
    return start


def reverse(l):
    return reverse_helper(l, cons_end)


def reverse_helper(l, rest):
    if l is cons_end:
        return rest
    return reverse_helper(cdr(l), cons(car(l), rest))


def nreverse(l):
    return nreverse_helper(l, cons_end)


def nreverse_helper(head, tail):
    if head is cons_end:
        return tail
    heads = cdr(head)
    setcdr(head, tail)
    return nreverse_helper(heads, head)


@native
def length(l):
    if l is cons_end:
        return 0
    if not isinstance(l, cons):
        return len(l)
    return 1 + length(cdr(l))


@native
def as_list(arg):
    r = cons_end
    for a in reversed(arg):
        r = cons(a, r)
    return r


def list_to_cons(l):
    r = cons_end
    for e in reversed(l):
        if isinstance(e, list):
            e = list_to_cons(e)
        r = cons(e, r)
    return r


@native
def is_callable(e):
    return is_function(e) or callable(e)


# Env, is_env, (env_d, env_parent), _ = __defstruct('Env', 'd', 'parent')


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


def env_contains(env, sym):
    assert is_symbol(sym), sym
    assert(is_env(env))
    #k = id(sym)
    k = sym
    while env is not None:
        d = env_d(env)
        if k in d:
            return True
        else:
            env = env_parent(env)
    return False


def env_get(env, sym):
    assert is_symbol(sym), sym
    assert(is_env(env))
    #k = id(sym)
    k = sym
    while env is not None:
        d = env_d(env)
        if k in d:
            return d[k]
        else:
            env = env_parent(env)
    # TODO bind
    raise KeyError(sym)


def env_containing_parent(env, sym):
    assert is_symbol(sym), sym
    assert(is_env(env))
    #k = id(sym)
    k = sym
    while env and k not in env_d(env):
        env = env_parent(env)
    return env


def env_def(env, sym, v):
    assert is_symbol(sym), sym
    assert(is_env(env))
    d = env_d(env)
    #k = id(sym)
    k = sym
    assert not k in d, '{k} in {d}'.format(k=sym, d=env_print_keys_values(env))
    #print('~~~~~~~~env_def:', k, '=', sexps_str(v), sexps_str(env_d(env)))
    env_d(env)[k] = v


def env_change(env, sym, v):
    assert is_symbol(sym), sym
    #k = id(sym)
    k = sym
    env = env_containing_parent(env, sym) or env
    #print('        env_change:', k, '=', sexps_str(v), env_d(env).keys())
    env_d(env)[k] = v
# from .base import __defstruct, py_bind_env


def env_print_keys(env):
    return ' '.join(sorted(map(symbol_name, env_d(env).keys())))


def env_print_keys_values(env):
    s = ['{k}: {v}'.format(k=symbol_name(k), v=repr(v)) for k, v in env_d(env).items()]
    return '{ %s }' % ', '.join(sorted(s))


symbols = {}
symbol_names = {}

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


def symbol_id_name(i):
    return symbol_names[i]


@native
def intern(s):
    assert(is_str(s)), s
    if s not in symbols:
        # i = get_interpreter_meta_level()
        sym = symbol(s)
        symbols[s] = sym
        symbol_names[id(sym)] = s
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

FAILED = 0
VALID = 1
SKIP = 2
RETURN = 4


def Valid(expr):
    return (VALID, expr)


def Skip():
    return (VALID | SKIP, None)


def Return(expr):
    return (VALID | RETURN, expr)


def ReturnSkip():
    return (VALID | RETURN | SKIP, None)


def Failed():
    return (FAILED, None)


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


def read_list(s):
    if stream_empty(s) or not is_paren_open(stream_next(s)):
        return Failed()
    else:
        # TODO check if ) throws error
        def read_list_end(s):
            if stream_empty(s) or not is_paren_close(stream_next(s)):
                return Failed()
            return ReturnSkip()

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
    return Skip() if parsed else Failed()


def read_comment(s):
    if stream_next(s) not in comment_chars:
        return Failed()
    while not stream_empty(s) and stream_peek(s) not in newlines:
        stream_next(s)
    return Skip()




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
            
            return Valid(list_to_cons([accessor_char_sym] + [intern(s) for s in accessors]))
    return Failed()
        

def read_quote_like(s, quote_char, quote_call_sym):
    for c in quote_char:
        if stream_next(s) != c:
            return Failed()
    # quote only supports one following exp
    expr = read(s, one=True)
    assert is_list(expr)
    r = cons(quote_call_sym, expr)
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


@native
def read(s, readers=readers, one=False):
    r = cons_end
    action = FAILED
    while not stream_empty(s) and not action & RETURN:
        for reader in readers:
            istart = stream_pos(s)
            action, res = reader(s)
            if action & VALID:
                if not action & SKIP:
                    r = cons(res, r)
                break
            else:
                stream_pos_set(s, istart)
        if not action & VALID:
            raise Exception('Unexpected: "%s" at %s' % (stream_peek(s), stream_pos(s)))
        # one reads until one actual token is parsed
        elif r and one:
            break
    if r is not None:
        assert is_list(r), r
        r = reverse(r)
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
class InternalException(Exception):
    def __init__(self, name, value=None):
        Exception.__init__(self, name)
        self.name = name
        self.value = value

@native
class BlockException(InternalException):
    pass


class StackTraceException(Exception):
    pass

def format_operator_call(fun, args):
    if not args:
        args = []
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


@native
def __block(env, name, *body):
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
    return is_list(p) and length(p) == 2 and is_symbol(p[0])


def is_simple_parameter(p):
    return is_symbol(p) and not is_keyword(p) and not is_special_keyword(p)


def is_normal_parameter(p):
    return is_simple_parameter(p) or is_parameter_with_default_(p)


def normal_parameter_name(p):
    if is_parameter_with_default_(p):
        return p[0]
    else:
        return p

function, is_function, (function_name, function_env, function_parameters, function_varargs_name, function_keysargs_name, function_nokeys, function_block_name, function_body), (function_set_name, _, _, _, _, _, _, _) = __defstruct('function', 'name', 'env', 'parameters', 'varargs_name', 'keysargs_name', 'nokeys', 'block_name', 'body')


def __fn(env, parameters, name=None, *body):

    valid_specials = {variadic_name, keys_name, nokeys_name}
    special_used = set()
    special_allows_next = {variadic_name, keys_name}
    # TODO support start_only specials
    special_end_only = {variadic_name, keys_name}
    special_once_only = {variadic_name, keys_name, nokeys_name}

    parsed_parameters = []

    special_param_names = {}
    special_param_names[variadic_name] = None
    special_param_names[keys_name] = None

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
            if param_special not in valid_specials:
                raise Exception(make_error_msg('Unknown special keyword: {s} at position {i}', s=p, i=i))
            if param_special in special_param_names:
                special_param_names[param_special] = True
            if param_special in special_allows_next:
                if has_normal_next:
                    # skip next parameter which we just parsed
                    i += 1
                    if param_special in special_param_names:
                        special_param_names[param_special] = next_param
                
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

    varargs_name = special_param_names[variadic_name]
    keysargs_name = special_param_names[keys_name]

    nokeys = nokeys_name in special_used

    user_function = function(name, env, parsed_parameters, varargs_name, keysargs_name, nokeys, block_name, body)

    #print('&&&&&&&&', special_used, sexps_str(parameters), nokeys_name in special_used)
    #assert is_callable(user_function), repr(user_function)
    return user_function


def __defun(env, name, parameters, *body):
    assert is_symbol(name), 'defun: {s}'.format(s=name)
    #if env_contains(env, name):
    #    raise Exception(make_error_msg('fun {fun} already declared', fun=symbol_name(name)))

    name_str = symbol_name(name)
    if parameters is None:
        parameters = []
    f = __fn(env, parameters, name, *body)
    env_def(env, name, f)
    return f


def __defmacro(lexical_env, name, parameters, *body):
    assert(is_symbol(name)), '{i}: {call}'.format(
        i=get_interpreter_meta_level()
        , call=format_operator_call('__defmacro', [lexical_env, name, parameters] + list(body)))
    if env_contains(lexical_env, name):
        raise Exception(make_error_msg('fun {fun} already declared', fun=symbol_name(name)))

    f = __fn(lexical_env, parameters, name, *body)
    m = macro(f)
    env_def(lexical_env, name, m)
    return m


def __apply(env, f_form, args):
    f = __eval(env, f_form)
    callstack.append((function_name(f) if is_function(f) else host_function_name(f), [args]))
    evaled_args = __eval(env, args)
    r = __call(env, f, evaled_args, do_eval_args=False)
    callstack.pop()
    return r
    

def __let(env, vars, *let_body):
    for var in vars:
        assert(is_list(var)), sexps_str(var)
        assert(len(var) == 2)
        assert (is_symbol(var[0])), sexps_str(var)

    let_env = make_env(env)

    # TODO support double assignment to same var
    assigned = set()
    for var in vars:
        name_sym, body = var
        val = __eval(let_env, body)
        # TODO we should have something like env_set
        setter = (env_def if not name_sym in assigned else env_change)
        setter(let_env, name_sym, val)
        assigned.add(name_sym)
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
        raise Exception(make_error_msg('var {var} already declared in global env {env}', var=repr(name), env=env_print_keys_values(env)))
    env_def(env, name, val)
    return val


def __setq(env, name, value):
    assert(env is not None)
    assert(is_symbol(name)), 'set: {s}'.format(s=name)
    #if not env_contains(env, name):
    #    raise Exception(make_error_msg('set: {sym} not declared in {env} ({is_env})'
    #                    , sym=symbol_name(name), env=sexps_str(env_d(env)), is_env=sexps_str(env_d(env_parent(env)) if env_parent(env) else '{}'))
    value = __eval(env, value)
    if is_function(value):
        function_set_name(value, name)
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
def host_function_name(fun):
    return fun.__name__

@native
def host_function_nokeys(fun):
    fun = get_native_function_id(fun)
    if fun not in native_functions:
        #parameters = py_get_param_names(fun)
        native_set_nokeys(fun, False)
        return False
    else:
        return native_functions[fun]


@native
def call_host_function(fun, args, kwargs):
    try:
        return fun(*args, **kwargs)
    except TypeError as e:
        raise Exception(make_error_msg('{e}\nfrom {call}\nwith kwargs: {kwargs}', e=e, call=format_operator_call(fun, args), kwargs=kwargs))


def call_function(fun, args_forms, nokeys, unevaled_args_forms):
    assert is_callable(fun), fun

    is_host_fun = False
    if is_function(fun):
        nokeys = nokeys or function_nokeys(fun)
    else:
        assert callable(fun), fun
        nokeys = nokeys or host_function_nokeys(fun)
        is_host_fun = True
        
    iparam = 0
    parsed_args = []
    kwargs = {}

    remaining_args = list(args_forms) if args_forms is not None else []
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
                raise Exception(make_error_msg(
                    'positional argument follows keyword argument {call}'.format(
                        call=format_operator_call(fun, args_forms))))

            parsed_args.append(arg)
        del arg


    if is_host_fun:
        return call_host_function(fun, parsed_args, kwargs)
    else:
        function_repr = function_name(fun) or '<fn>'
        parameters = function_parameters(fun)
        set_varargs = function_varargs_name
        set_kwargs = function_keysargs_name
        
        def call_make_error(err):
            return err + '''
in function call:
    {call}
evaled to:
    {evaled_args} 
parsed as:
    {parsed_args}
function expects:
    {params} &rest {varargs} &keys {kwargs}.'''.format(
        call=format_operator_call(function_repr, unevaled_args_forms)
        , parsed_args=format_operator_call(function_repr, parsed_args)
        , evaled_args=format_operator_call(function_repr, args_forms)
        , kwargs=set_kwargs
        , params=sexps_str(parameters)
        , varargs=repr(set_varargs)
        )

        if not set_varargs and len(parsed_args) > len(parameters):
            raise Exception(make_error_msg(call_make_error('too many arguments (#{n} vs #{m})'.format(
            n=len(parameters)
                , m=len(parsed_args)
            ))))

        # try defaults, extract missing positional args from kwargs
        if len(parsed_args) < len(parameters):
            for i, p in enumerate(parameters[len(parsed_args):]):
                assert(isinstance(p, tuple)), p
                (param_name, param_default) = p
                n = symbol_name(param_name)
                in_kwargs = n in kwargs
                if not in_kwargs and param_default is None:
                    raise Exception(make_error_msg(call_make_error('function call missing argument "{name}"{default}'.format(name=n, default='(:= {d})'.format(d=param_default()) if param_default else ''))))
                                    
                parsed_args.append(kwargs[n] if in_kwargs else param_default())
                if in_kwargs:
                    del kwargs[n]

        # even afer adding default values still not enough args
        if len(parameters) > len(parsed_args):
            raise Exception(make_error_msg('function call missing argument "{i}": {call}'
                                           , i=symbol_name(parameters[len(parsed_args)][0]), fun=format_operator_call(function_repr, unevaled_args_forms)))

        if not set_kwargs and kwargs:
            raise Exception(make_error_msg('Unexpected keyword arguments for function call: ({fun} {params} {kwargs}) called with {args}'
                                           , fun=function_repr, args=sexps_str(parsed_args_forms), params=sexps_str(parameters), kwargs=sexps_str(kwargs)))

        varargs = []
        if set_varargs and len(parsed_args) > len(parameters):
            varargs = parsed_args[len(parameters):]
            parsed_args = parsed_args[:len(parameters)]

        def user_function(fun, args, varargs, kwargs):
            fun_def_env = function_env(fun)
            fun_env = make_env(fun_def_env)

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
            return __block(fun_env, block_name, *body)

        # force cons for varargs
        return user_function(fun, parsed_args, as_list(varargs), kwargs)


def __call_function(env, fun, args_forms, eval):
    nokeys = False

    if is_list(args_forms):
        args_forms = list(args_forms)

    first_arg = args_forms[0] if args_forms else None
    if first_arg == nokeys_sym:
        nokeys = True
        args_forms.pop(0)

    unevaled_args_forms = args_forms
    # TODO: if we parse after eval, we can't compile?
    # apply might already have evaled arguments
    if eval and args_forms is not None:
        args_forms = [__eval(env, arg) for arg in args_forms]

    return call_function(fun, args_forms, nokeys, unevaled_args_forms)


def __macroexpand_1(env, fun, args_forms):
    return __call_function(env, fun, args_forms, eval=False)


def __macroexpand(env, fun, args_forms):
    form = __macroexpand_1(env, fun, args_forms)
    while is_operator_call(form) and is_macro(car(form)):
        form = __macroexpand_1(env, *form)
    return form
    

def __call(env, fun, args_forms, do_eval_args):
    if is_special_form(fun):
        fun = special_form_fun(fun)
        debug(lambda: repr(fun))
        r = __macroexpand_1(env, fun, cons(env, args_forms))
        return r
    elif is_macro(fun):
        fun = macro_fun(fun)
        form = __macroexpand(env, fun, args_forms)
        if form is None:
            return form
        r = __eval(env, form)
        return r
    elif is_callable(fun):
        return __call_function(env, fun, args_forms, do_eval_args)
    else:
        raise Exception(make_error_msg('({fun} {args}) is not callable', fun=repr(fun), args=sexps_str(args_forms) if args_forms else ''))


def is_operator_call(form):
    return is_list(form) and form


def __eval(env, form):
    debug('******** eval :', lambda: sexps_str(form))
    r = None
    if is_symbol(form) and not is_keyword(form):
        if not env_contains(env, form):
            def print_env_keys(env):
                r = []
                while env is not None:
                    r += ['Keys: ' + env_print_keys(env)]
                    env = env_parent(env)
                return '\n'.join(r)
                    

            raise Exception(make_error_msg('Symbol "{sym}" not found in env \n{keys}'
                                           , sym=symbol_name(form)
                                           , keys=print_env_keys(env)))
        r = env_get(env, form)
    elif is_atom(form):
        r = form
    elif is_operator_call(form):
        args_forms = cdr(form)
        callstack.append((car(form), args_forms))
        fun = __eval(env, car(form))
        assert is_macro(fun) or is_special_form(fun) or is_callable(fun), fun
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

    # list basics
    bindn('as-list', 'as_list', as_list)

    bind('cons', cons)
    bind('car', car)
    bind('cdr', cdr)
    
    bind('setcar', setcar)
    bind('setcdr', setcdr)

    bind('reverse', reverse)
    bind('nreverse', nreverse)

    bind('cons:append', cons_append)

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

    bind('Env', Env)
    bind('is_env', is_env)
    bind('env_d', env_d)
    bind('env_parent', env_parent)


    @native
    def dict_set(d, k, v):
        d[k] = v
    bindn('dict-set', 'dict_set', dict_set)

    @native
    def dict_keys(d):
        return as_list(d.keys())
    bindn('dict-keys', 'dict_keys', dict_keys)


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
        
    bind('internal:block', special_form(__block))
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
            return as_list(args)
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

    # special form because we need the env
    def source_eval(env, form):
        form = __eval(env, form)
        return __eval(env, form)
    bind('eval', special_form(source_eval))

    def macroexpand1(env, form):
        form = __eval(env, form)
        assert is_operator_call(form), form
        fun = __eval(env, car(form))
        assert is_macro(fun), fun
        fun = macro_fun(fun)
        return __macroexpand_1(env, fun, cdr(form))
        
    def macroexpand(env, form):
        form = __eval(env, form)
        assert is_operator_call(form), form
        fun = car(form)
        assert is_macro(fun), fun
        fun = macro_fun(__eval(env, fun))
        return __macroexpand(env, fun, cdr(form))

    bind('macroexpand-1', special_form(macroexpand1))
    bind('macroexpand', special_form(macroexpand))
    bind('set', special_form(__setq))
    bind('progn', special_form(__progn))

    bind('def', special_form(__def))
    bind('defun', special_form(__defun))
    bind('defmacro', special_form(__defmacro))
    bind('fn', special_form(lambda env, parameters, *body: __fn(env, parameters if parameters else [], None, *body)))
    bind('apply', special_form(__apply))

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
            if is_vec(l):
                return l[istart:iend]
            else:
                l = nthcdr(istart, l)
                r = cons_end
                for i in range(iend - istart):
                    assert l is not cons_end, l
                    e = car(l)
                    l = cdr(l)
                    r = cons(e, r)
                return reverse(r)
        else:
            if istart is None:
                istart = 0
            iend, step = args
            if iend is None:
                iend = len(l)
            if step is None:
                step = 1
            # FIXME does this work with cons?
            r = [l[i] for i in range(istart, iend, step)]
            if is_str(l):
                return ''.join(r)
            elif is_list(l):
                return as_list(r)
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
        assert (is_list(l) or is_tuple(l) or is_str(l) or isinstance(l, list)),  'nth: {i} {l} ({t})'.format(i=sexps_str(i), l=sexps_str(l), t=type(l))
        assert len(l) > i, (i, len(l))
        return l[i]
    bind('nth', nth)

    def is_vec(l):
        return isinstance(l, list)

    bind('vec?', is_vec)

    def vec_set(l, i, v):
        assert is_vec(l), l
        l[i] = v
    bind('vec-set', vec_set)

    bind('defstruct', special_form(defstruct))
    bind('__defstruct', __defstruct)


    def throw(env, tag, value):
        tag = __eval(env, tag)
        value = __eval(env, value)
        raise InternalException(tag, value)
    bind('throw', special_form(throw))

    def catch(env, tag, *body):
        tag = __eval(env, tag)

        try:
            return __progn(env, *body)
        except InternalException as e:
            if e.name is tag:
                return e.value
            else:
                raise e
    bind('catch', special_form(catch))

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

    def file_read(filename):
        with open(filename, 'r') as f:
            return f.read()
    bind('file-read', file_read)
    
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

    bind('host_function_name', function_name)
    bind('is_host_function', is_function)
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
    except StackTraceException as e:
        raise e
    except Exception as e:
        raise StackTraceException(make_error_msg('{E}: {e}', E=type(e).__name__, e=str(e)))
    except KeyboardInterrupt as e:
        raise StackTraceException(make_error_msg('{E}: {e}', E=type(e).__name__, e=str(e)))

interpret = _interpret
#interpret = native_interpret

# from .base import TYPE, TYPE_T
# from .symbol import intern
# from .reader import read, Stream, quote_fun_name, backquote_fun_name, backquote_eval_fun_name, backquote_splice_fun_name
# from .interpreter import interpret, ps

import unittest
import argparse

reader_tests = [
    ('''''', None)
    , ('1', cons(1, None))
    , ('1.0', cons(1.0, None))
    , ('''()''', cons(None, None))
    , ('''(1)''', list_to_cons([[1]]))
    , ('''(1 2)''', list_to_cons([[1, 2]]))
    , ('''(foo)''', list_to_cons([[intern('foo')]]))
    , ('''(foo bar)''', list_to_cons([[intern('foo'), intern('bar')]]))
    , ('''(1+)''', list_to_cons([[intern('1+')]]))
    , ('''(())''', list_to_cons([[[]]]))
    , ('''((list a b ()))''', list_to_cons([[[intern('list'), intern('a'), intern('b'), []]]]))
    , ("'a", list_to_cons([[intern(quote_fun_name), intern('a')]]))
    , ("'()", list_to_cons([[intern(quote_fun_name), []]]))
    , ("()1", list_to_cons([[], 1]))
    , ("'()1", list_to_cons([[intern(quote_fun_name), []], 1]))
    , ("`()", list_to_cons([[intern(backquote_fun_name), []]]))
    , ("`~foo", list_to_cons([[intern(backquote_fun_name), [intern(backquote_eval_fun_name), intern('foo')]]]))
    , ("`(~foo)", list_to_cons([[intern(backquote_fun_name), [[intern(backquote_eval_fun_name), intern('foo')]]]]))
    , ("`(bar ~foo)", list_to_cons([[intern(backquote_fun_name), [intern('bar'), [intern(backquote_eval_fun_name), intern('foo')]]]]))
    , ("`(bar ~foo baz)", list_to_cons([[intern(backquote_fun_name), [intern('bar'), [intern(backquote_eval_fun_name), intern('foo')], intern('baz')]]]))
    , ("`(bar ~@ foo baz)", list_to_cons([[intern(backquote_fun_name), [intern('bar'), [intern(backquote_splice_fun_name), intern('foo')], intern('baz')]]]))
    , ("a.b", list_to_cons([[intern('.'), intern('a'), intern('b')]]))
    , (";a", list_to_cons([]))
    , ("""(;)a
)""", list_to_cons([[]]))
    , ("'() ;1", list_to_cons([[intern(quote_fun_name), []]]))
    , ("(defun foo () (+ 1 2)) (foo)", list_to_cons([[intern('defun'), intern('foo'), [], [intern('+'), 1, 2]], [intern('foo')]]))
    , ("""(set p ((. a b)))""", list_to_cons([[intern('set'), intern('p'), [[intern('.'), intern('a'), intern('b')]]]]))
]


def get_process_args():
    print('Getting process args', argv)
    return argv

@native
def get_process_args():
    import sys
    #print('getting process args', sys.argv)
    return sys.argv[1:]



p = argparse.ArgumentParser()
p.add_argument('--type')
p.add_argument('--num', type=int)
p.add_argument('-c', nargs='+')
p.add_argument('args', nargs='*')
p.add_argument('--debug', action='store_true')

args = p.parse_args(get_process_args())

if not args.debug:
    interpret = native_interpret
    

tests_file = 'tests.lisp'

def load_tests():
    tests = None
    with open(tests_file, 'r') as f:
        tests = f.read()
    tests = read(Stream(tests, 0))
    return tests

if args.type is not None and args.num is not None:
    if args.type == 'reader':
        program = reader_tests[args.num][0]
        ps(program)
        ps(read(Stream(program, 0)))
    if args.type == 'interpreter':
        tests = load_tests()
        program = tests[args.num][0]
        print('Program:', sexps_str(program))
        ps(interpret(program))
elif args.c:
    for filename in args.c:
        with open(filename, 'r') as f:
            program = f.read()
        print('Result:', interpret(read(Stream(program, 0)), args=args.args))
else:
    for program, expected in reader_tests:
        print('READER-TEST', program)
        r = read(Stream(program, 0))
        assert r == expected, (r, expected)
                 

    tests = load_tests()

    test_results = []

    def run_test(test, result):
        r = interpret(test)
        return result, r, result

    # @native
    # def run_test(test, result):
    #     try:
    #         r = interpret(test)
    #         return r == result, r, result
    #     except Exception as e:
    #         import traceback
    #         r = traceback.format_exc()
    #         return False, r, result

    for itest, (test, result) in list(enumerate(tests))[istart:]:
        print('================ RUNNING test {i} ================'.format(i=itest))
        print(sexps_str(test))
        result = interpret([result])
        r = run_test(test, result)
        test_results.append(r)
        assert r[1] == r[2], '''
{a}
!=
{b}
i.e.
{aa}
!= 
{bb}'''.format(a=repr(r[1]), b=repr(result), aa=sexps_str(r[1]), bb=sexps_str(result))

    for itest, (success, r, expected) in reversed(list(enumerate(test_results))):
        if not success:
            print('''^^^^ Test {i} failed. ^^^^
    {test}
expected:
    {expected}
but got:
    {r}'''.format(i=itest, test=sexps_str(tests[itest][0]), expected=sexps_str(expected), r=r))
            if isinstance(r, Exception):
                traceback.print_tb(r.__traceback__)
    print('{nfailed} of {n} tests succeeded.'.format(nfailed=len([1 for success, r in test_results if success]), n=len(test_results)))
                      


