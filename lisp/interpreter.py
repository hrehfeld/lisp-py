from .base import native, Struct
from .symbol import intern, Symbol, symbol_name, symbolp, gensym
from .reader import read, Stream, quote_fun_name, backquote_fun_name, backquote_eval_fun_name, backquote_splice_fun_name
import operator

class BlockException(Exception):
    def __init__(self, name, value=None):
        Exception.__init__(self, name)
        self.name = name
        self.value = value


def named_operatorp(form, op):
    assert(symbolp(op))
    return listp(form) and form and symbolp(form[0]) and form[0] == op


def sexps_str(form, indent=0):
    def p(f):
        return ('  ' * indent + str(f) + '\n')

    r = ''
    if named_operatorp(form, intern('quote')):
        r += ' '.join(["'" + sexps_str(f) for f in form[1:]])
    elif isinstance(form, list) or isinstance(form, tuple):
        r += p('(')
        for e in form:
            r += sexps_str(e, indent + 1)
        r += p(')')
    elif symbolp(form):
        r += p(symbol_name(form))
    elif isinstance(form, dict):
        r += p('{')
        for e, v in form.items():
            r += sexps_str(e, indent + 1) + ': ' + sexps_str(e, indent + 1)
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


MACRO = '__macro'
SPECIAL_FORM = '__special'

variadic_name = '&rest'
keys_name = '&keys'


def __defstruct(env, name, *fields):
    name_str = symbol_name(name)
    field_names = [symbol_name(f) for f in fields]

    constructor, instancep, getter, setter = Struct(name_str, *field_names)
    env_def(env, name_str, constructor)
    fname = '%s?' % (name_str)
    env_def(env, fname, instancep)
    for field, get in zip(field_names, getter):
        fname = '%s-%s' % (name_str, (field))
        env_def(env, fname, get)

    for field, set in zip(field_names, setter):
        fname = '%s-%s-set' % (name_str, (field))
        env_def(env, fname, set)

    return constructor


def special_formp(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == SPECIAL_FORM


def special_form(f):
    if not callablep(f):
        raise Exception('%s is not callable' % (f))
    return (SPECIAL_FORM, f)


def special_form_get_fun(f):
    return f[1]


def macrop(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == MACRO


def Macro(f):
    if not callablep(f):
        raise Exception('%s is not callable' % (f))

    return (MACRO, f)


def macro_get_fun(macro):
    return macro[1]


functions = {}


def add_function(f, parameters, defaults, special_names, special_defaults):
    functions[f] = (parameters, defaults, special_names, special_defaults)


@native
def block(env, name, *body):
    try:
        r = __progn(env, *body)
    except BlockException as e:
        if e.name != name:
            raise e
        return e.value
    return r


@native
def return_from(env, name, value=None):
    r = __eval(env, value)
    raise BlockException(name, r)
    

def __fn(env, parameters, *body):
    def parameter_default(p):
        return p[1]

    def parameter_with_default_p(p):
            return isinstance(p, list) and len(p) == 2

    def simple_parameterp(p):
        return symbolp(p) and not keywordp(p) and not special_keywordp(p)

    def normal_parameterp(p):
        return simple_parameterp(p) or parameter_with_default_p(p)

    def normal_parameter_name(p):
        if parameter_with_default_p(p):
            return p[1]
        else:
            return p

    special_names = {variadic_name: None, keys_name: None}
    special_defaults = {variadic_name: None, keys_name: None}
    ilast_special = None
    i = len(parameters) - 1
    while i >= 0 and (ilast_special is None or ilast_special - i <= 2):
        p = parameters[i]
        if special_keywordp(p):
            name = symbol_name(p)
            if name in special_names:
                assert(special_names[name] is None)
                nextp = parameters[i + 1] if i + 1 < len(parameters) else None
                if nextp and normal_parameterp(nextp):
                    special_names[name] = normal_parameter_name(nextp)
                    if parameter_with_default_p(nextp):
                        special_defaults[name] = parameter_default(nextp)
                else:
                    special_names[name] = True
                ilast_special = i
            else:
                raise Exception('Unkown special keyword: {s} at position {i}'.format(s=p, i=i))
        i -= 1

    parameters = parameters[:ilast_special]

    for i, p in enumerate(parameters):
        assert normal_parameterp(p), p

    # TODO check if parameters with defaults are after normal ones
    defaults = {}
    defaults_started = False
    for i, p in enumerate(parameters):
        if not symbolp(p):
            parameters[i] = normal_parameter_name(p)
            defaults[i] = parameter_default(p)
            defaults_started = True
        elif defaults_started:
            raise Exception('parameters with defaults need to come after normal ones. {ps}'.format(ps=parameters))

    block_name = gensym()
    def f(args, varargs, kwargs):
        fun_env = Env(env)
        env_def(fun_env, 'return', lambda value=None: return_from(fun_env, block_name, value))

        for parameter, arg in zip(parameters, args):
            env_def(fun_env, symbol_name(parameter), arg)

        varargs_name = special_names[variadic_name]
        if varargs_name:
            env_def(fun_env, symbol_name(varargs_name), varargs)
        keysargs_name = special_names[keys_name]
        if keysargs_name:
            env_def(fun_env, symbol_name(keysargs_name), kwargs)

        return block(fun_env, block_name, *body)
    add_function(f, parameters, defaults, special_names, special_defaults)
    return f


def __defun(env, name, parameters, *body):
    assert(symbolp(name))
    if env_contains(env, symbol_name(name)):
        raise Exception('fun %s already declared' % symbol_name(name))

    f = __fn(env, parameters, *body)
    env_def(env, symbol_name(name), f)
    return f


def __defmacro(lexical_env, name, parameters, *body):
    assert(symbolp(name)), (name, type(name))
    if env_contains(lexical_env, symbol_name(name)):
        raise Exception('fun %s already declared' % symbol_name(name))

    f = __fn(lexical_env, parameters, *body)
    m = Macro(f)
    env_def(lexical_env, symbol_name(name), m)
    return m


def __funcall(env, f, *args):
    f = __eval(env, f)
    return __call(env, f, args)
    

def __apply(env, f, args):
    f = __eval(env, f)
    args = __eval(env, args)
    return __call(env, f, args)
    

def __let(env, vars, *let_body):
    for var in vars:
        assert(listp(var))
        assert(len(var) == 2)

    let_env = Env(env)

    for var in vars:
        name_sym, body = var
        val = __eval(let_env, body)
        env_def(let_env, symbol_name(name_sym), val)
    return __progn(let_env, *let_body)


# TODO if without else
def __if(env, cond, then, else_body=None):
    cond = __eval(env, cond)
    if cond:
        r = __eval(env, then)
    elif else_body:
        r = __progn(env, else_body)
    else:
        r = None
    return r


def is_num(f):
    return isinstance(f, int) or isinstance(f, float)


def is_str(f):
    return isinstance(f, str)


def atomp(form):
    return is_num(form) or is_str(form) or keywordp(form) or symbolp(form) or (listp(form) and not len(form))


def keywordp(e):
    return symbolp(e) and symbol_name(e).startswith(':')


def special_keywordp(e):
    return symbolp(e) and symbol_name(e).startswith('&')


def listp(e):
    return isinstance(e, list)


def length(e):
    assert(isinstance(e, list))
    return len(e)


def callablep(e):
    return callable(e)


def __def(env, name, *args):
    assert(symbolp(name))
    if env_contains(env, symbol_name(name)):
        raise Exception('var %s already declared' % symbol_name(name))
    val = __eval(env, args[0]) if args else None
    env_def(env, symbol_name(name), val)
    return val


def __setq(env, name, *args):
    assert(env is not None)
    assert(symbolp(name))
    if not env_contains(env, symbol_name(name)):
        raise Exception('set: {sym} not declared in {env} ({envp})'
                        .format(sym=symbol_name(name), env=sexps_str(env.d), envp=sexps_str(env.parent.d) if env.parent else '{}'))
    val = __eval(env, args[0]) if args else None
    env_change(env, symbol_name(name), val)
    return val


def __call_function(env, fun, args_forms, eval=True):
    kw = False
    args = []
    varargs = []
    ilast_normal_arg = -1
    for iarg, arg in enumerate(args_forms):
        if keywordp(arg):
            kw = arg
        else:
            if kw:
                # without :
                k = symbol_name(kw)[1:]
                arg = (k, arg)
            else:
                ilast_normal_arg = iarg
            args += [arg]
    del kw

    if fun not in functions:
        # python fun

        clean_args = []
        kwargs = {}
        for iarg, arg in enumerate(args):
            if iarg > ilast_normal_arg:
                k, v = arg
                kwargs[k] = v
            else:
                # TODO hack: we really need to fix our type system
                if isinstance(arg, tuple) and not symbolp(arg):
                    k, v = arg
                else:
                    v = arg
                clean_args += [v]

        args = clean_args
        del clean_args

        if eval:
            args = [__eval(env, f) for f in args]
            for k, v in kwargs.items():
                kwargs[k] = __eval(env, v)

        return fun(*args, **kwargs)
    else:
        # self-defined fun
        (parameters, defaults, special_names, special_defaults) = functions[fun]

        args_dict = dict()
        args_dict.update(defaults)


        def parameter_index(k):
            for i, p in enumerate(parameters):
                if symbol_name(p) == k:
                    return i
            return None

        kwargs = {}
        for iarg, arg in enumerate(args):
            # TODO we really need a type system to distinguish between symbols and tuples
            if isinstance(arg, tuple) and not symbolp(arg):
                k, v = arg
                i = parameter_index(k)
                if i is None:
                    kwargs[k] = v
                else:
                    args_dict[i] = v
            else:
                v = arg
                i = iarg
                args_dict[i] = v
            del arg

        for i, p in enumerate(parameters):
            if not (i in args_dict):
                raise Exception('function call missing argument #{i} {name}: ({fun} {args})'
                                .format(i=i, name=symbol_name(p), fun=fun, args=sexps_str(args_forms)))

        if kwargs and special_names[keys_name] is None:
            raise Exception('unknown keyword args: {kws}'.format(kws=kwargs))

        args = []
        for i, p in enumerate(parameters):
            args += [args_dict[i]]

        num_args = len(args_dict)
        if num_args != len(parameters) and special_names[variadic_name] is None:
            raise Exception('too many arguments for call to {fun}'.format(fun=fun))

        varargs = []
        for i in range(len(parameters), num_args):
            varargs += [args_dict[i]]

        if eval:
            args = [__eval(env, f) for f in args]
            varargs = [__eval(env, f) for f in varargs]
            for k, v in kwargs.items():
                kwargs[k] = __eval(env, v)

        return fun(args, varargs, kwargs)
    

def __call(env, fun, args_forms):
    if special_formp(fun):
        fun = special_form_get_fun(fun)
        return __call_function(env, fun, [env] + args_forms, eval=False)

    elif macrop(fun):
        fun = macro_get_fun(fun)
        form = __call_function(env, fun, args_forms, eval=False)
        return __eval(env, form)

    elif callablep(fun):
        return __call_function(env, fun, args_forms)

    else:
        raise Exception('({fun} {args}) is not callable'.format(fun=fun, args=sexps_str(args_forms) if args_forms else ''))


def __eval(env, form):
    if symbolp(form):
        if not env_contains(env, symbol_name(form)):
            raise Exception('Symbol "%s" not found in env (Keys: %s => %s)' % (symbol_name(form), ', '.join(env.d.keys()), ', '.join(env.parent.d.keys()) if env.parent else ''))
        return env_get(env, symbol_name(form))
    elif listp(form):
        if not length(form):
            raise Exception('trying to evaluate list of length 0')
        fun = __eval(env, form[0])
        args_forms = form[1:]
        
        return __call(env, fun, args_forms)
    elif atomp(form):
        return form
    else:
        raise Exception('unknown form: {form}'.format(form=sexps_str(form)))
        

class Env:
    def __init__(self, parent=None):
        self.parent = parent
        self.d = {}


def env_contains(self, k):
    return k in self.d or (self.parent and env_contains(self.parent, k))


def env_get(env, k):
    if k in env.d:
        return env.d[k]
    elif env.parent:
        return env_get(env.parent, k)
    else:
        raise KeyError(k)


def env_containing_parent(env, k):
    while env and k not in env.d:
        env = env.parent
    return env


def env_def(env, k, v):
    assert not env_contains(env, k)
    env.d[k] = v


def env_change(env, k, v):
    env = env_containing_parent(env, k) or env
    env.d[k] = v


def base_env(args=[]):
    env = Env()

    env_def(env, 'true', True)
    env_def(env, 'false', False)
    env_def(env, 'nil', None)
    def list_(*args):
        return list(args)
    env_def(env, 'list', list_)

    def append(l, *es):
        for e in es:
            l.append(e)
        return l
    env_def(env, 'append', append)

    def extend(l, *ls):
        l = list(l)
        for e in ls:
            l += e
        return l
    env_def(env, 'extend', extend)

    env_def(env, 'str', str)
    env_def(env, 'repr', repr)

    env_def(env, 'dict', dict)
    env_def(env, 'dict-setdefault', dict.setdefault)
    env_def(env, 'dict-get', dict.get)
    def dict_set(d, k, v):
        d[k] = v
    env_def(env, 'dict-set', dict_set)

    def Tuple(*args):
        return tuple(args)
    env_def(env, 'tuple', Tuple)


    def __assert(env, cond, msg=''):
        msg = '%s: %s' % (sexps_str(cond), msg)
        r = __eval(env, cond)
        assert r, msg
    env_def(env, 'assert', special_form(__assert))
    
    def __import(env, *args):
        # TODO 
        pass
    env_def(env, 'import', special_form(__import))

    def __while(env, cond, *body):
        while __eval(env, cond):
            __eval(env, [intern('progn'), *body])
    env_def(env, 'block', special_form(block))
    env_def(env, 'return-from', special_form(return_from))

    env_def(env, 'while', special_form(__while))

    def __lookup(env, obj, *ks):
        r = __eval(env, obj)
        for k in ks:
            r = getattr(r, symbol_name(k))
        return r
    env_def(env, '.', special_form(__lookup))

    env_def(env, quote_fun_name, special_form(lambda env, e: e))

    def backquote_evalp(s):
        return named_operatorp(s, intern(backquote_eval_fun_name)) or named_operatorp(s, intern(backquote_splice_fun_name))
    
    def backquote_(env, s, level):
        if atomp(s):
            return [s]
        elif named_operatorp(s, intern(backquote_eval_fun_name)):
            assert(len(s) == 2)
            # nested backquote eval
            if backquote_evalp(s[1]):
                return [s[1]]
            return [__eval(env, nth(1, s))]
        elif named_operatorp(s, intern(backquote_splice_fun_name)):
            assert(len(s) == 2)
            if backquote_evalp(s[1]):
                return [s[1]]
            return __eval(env, nth(1, s))
        elif named_operatorp(s, intern(backquote_fun_name)):
            assert(len(s) > 1)
            r = backquote_(env, s[1:], level + 1)
            assert(len(r) == 1)
            return [[intern(backquote_fun_name)] + r[0]]
        elif listp(s):
            r = []
            for e in s:
                r += backquote_(env, e, level)
            return [r]
        else:
            raise Exception(sexps_str(s))

    def backquote(env, s):
        r = backquote_(env, s, 0)
        assert(len(r) == 1)
        r = r[0]
        return r

    env_def(env, backquote_fun_name, special_form(backquote))
    env_def(env, 'eval', special_form(__eval))
    env_def(env, 'set', special_form(__setq))
    env_def(env, 'let', special_form(__let))
    env_def(env, 'progn', special_form(__progn))

    env_def(env, 'def', special_form(__def))
    env_def(env, 'defun', special_form(__defun))
    env_def(env, 'defmacro', special_form(__defmacro))
    env_def(env, 'fn', special_form(__fn))
    env_def(env, 'call', special_form(__funcall))
    env_def(env, 'apply', special_form(__apply))
    env_def(env, 'if', special_form(__if))

    env_def(env, 'gensym', gensym)

    env_def(env, 'symbolp', symbolp)
    env_def(env, 'symbol?', symbolp)

    env_def(env, 'listp', listp)
    env_def(env, 'list?', listp)

    def tuplep(v):
        return isinstance(v, tuple)
    env_def(env, 'tuplep', tuplep)
    env_def(env, 'tuple?', tuplep)

    env_def(env, 'named-operator?', named_operatorp)

    def numeric_op(op):
        def numeric_op(a, *args):
            r = a
            for b in args:
                r = op(r, b)
            return r
        return numeric_op

    env_def(env, '+', numeric_op(operator.__add__))
    env_def(env, '-', numeric_op(operator.__sub__))
    env_def(env, '*', numeric_op(operator.__mul__))
    env_def(env, '/', numeric_op(operator.__truediv__))

    env_def(env, 'eq', operator.__eq__)
    env_def(env, 'neq', operator.__ne__)

    env_def(env, 'is', lambda a, *bs: all([a is b for b in bs]))

    env_def(env, 'not', operator.__not__)
    env_def(env, 'and', operator.__and__)
    env_def(env, 'or', operator.__or__)

    env_def(env, '<', operator.__lt__)
    env_def(env, '<=', operator.__le__)
    env_def(env, '>', operator.__gt__)
    env_def(env, '>=', operator.__ge__)

    def cons(e, l):
        assert listp(l), repr(l)
        return [e] + l
    env_def(env, 'cons', cons)

    def length(l):
        return len(l)
    env_def(env, 'length', length)

    def has(l, e):
        return e in l
    env_def(env, 'contains?', has)

    def nth(i, l):
        return l[i]
    env_def(env, 'nth', nth)

    def tail(l):
        return l[1:]
    env_def(env, 'tail', tail)
    
    env_def(env, 'defstruct', special_form(__defstruct))


    def throw(e):
        raise e
    env_def(env, 'throw', throw)

    def exception(s):
        return Exception(s)
    env_def(env, 'Exception', exception)
    
    # sys utils
    def file_open(filename, mode):
        assert(symbolp(mode))
        mode = symbol_name(mode)
        return open(filename, mode)

    env_def(env, 'file-open', file_open)
    
    env_def(env, 'argv', args)

    import pathlib
    env_def(env, 'make-Path', pathlib.Path)

    def print_(*args):
        print(*args)
    env_def(env, 'print', print_)

    def princ(arg):
        ps(arg)
    env_def(env, 'princ', princ)

    # TODO
    #(infix

    with open('stdlib.lisp', 'r') as f:
        interpret(read(Stream(f.read(), 0)), env)

    return env


def __progn(env, *forms):
    r = None
    for form in forms:
        r = __eval(env, form)
    return r


def interpret(forms, env=None, args=[]):
    if env is None:
        env = base_env(args)
    return __progn(env, *forms)
    
