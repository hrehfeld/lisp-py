from .symbol import intern, Symbol, symbol_name

import operator


MACRO = '__macro'
SPECIAL_FORM = '__special'


class Struct:
    def __init__(self, *values):
        self.values = list(values)

    def __eq__(self, o):
        return type(self) == type(o) and self.values == o.values


def make_struct(env, name, *fields):
    def constructor(*values):
        assert(len(fields) == len(values))
        return Struct(*values)

    env['make-%s' % symbol_name(name)] = constructor
    for ifield, field in enumerate(fields):
        env['%s-%s' % (symbol_name(name), symbol_name(field))] = lambda struct: struct.values[ifield]

    return constructor

def special_formp(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == SPECIAL_FORM


def special_form(f):
    if not callablep(f):
        raise Exception('%s is not callable' % (f))
    return (SPECIAL_FORM, f)


def macrop(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == MACRO


def Macro(f):
    if not callablep(f):
        raise Exception('%s is not callable' % (f))

    return (MACRO, f)


def fn(env, parameters, *body):
    for i, parameter in enumerate(parameters):
        assert(symbolp(parameter))

    variadic_name_sym = None
    for i in (1, 2):
        if len(parameters) >= i and parameters[-i] == intern('&'):
            if i == 2:
                variadic_name_sym = parameters[-1]
            parameters = parameters[:-i]
            break

    def f(*args):
        fun_env = Env(parent=env)

        for name_sym, val in zip(parameters, args):
            fun_env[symbol_name(name_sym)] = val

        if variadic_name_sym is not None:
            var_args = []
            if len(args) > len(parameters):
                var_args = args[len(parameters):]
            fun_env[symbol_name(variadic_name_sym)] = var_args
        else:
            assert(len(args) == len(parameters))
        return progn(fun_env, body)
    return f


def defun(env, name, parameters, *body):
    assert(symbolp(name))
    if symbol_name(name) in env:
        raise Exception('fun %s already declared' % symbol_name(name))

    f = fn(env, parameters, *body)
    env[symbol_name(name)] = f
    return f


def defmacro(lexical_env, name, parameters, *body):
    assert(symbolp(name))
    if symbol_name(name) in lexical_env:
        raise Exception('fun %s already declared' % symbol_name(name))

    f = fn(lexical_env, parameters, *body)
    m = Macro(lambda dynamic_env, *args: f(*args))
    lexical_env[symbol_name(name)] = m
    return m


def funcall(env, f, *args):
    f = eval(env, f)
    args = [eval(env, a) for a in args]
    return f(*args)
    

def apply(env, f, args):
    f = eval(env, f)
    args = eval(env, args)
    return f(*args)
    

# TODO if without else
def _if(env, cond, then, _else):
    cond = eval(env, cond)
    # TODO check trueness test
    body = then if cond else _else
    return progn(env, [body])


def is_num(f):
    return isinstance(f, int) or isinstance(f, float)


def symbolp(e):
    return isinstance(e, Symbol)


def listp(e):
    return isinstance(e, list)


def length(e):
    assert(isinstance(e, list))
    return len(e)


def callablep(e):
    return callable(e)


def set_var(env, name, args):
    assert(len(args) <= 1)
    val = eval(env, args[0]) if args else None
    env[symbol_name(name)] = val
    return val


def defq(env, name, *args):
    assert(symbolp(name))
    if symbol_name(name) in env:
        raise Exception('var %s already declared' % symbol_name(name))
    return set_var(env, name, args)


def setq(env, name, *args):
    assert(symbolp(name))
    if symbol_name(name) not in env:
        raise Exception('var %s not declared' % symbol_name(name))
    return set_var(env, name, args)


def eval(env, form):
    if is_num(form):
        return form
    if symbolp(form):
        if not symbol_name(form) in env:
            raise Exception('Symbol %s not found in env (Keys: %s)' % (symbol_name(form), ', '.join(env.d.keys())))
        return env[symbol_name(form)]
    if listp(form):
        if not length(form):
            raise Exception('trying to evaluate list of length 0')
        fun = eval(env, form[0])
        args_forms = form[1:]

        if special_formp(fun):
            return fun[1](env, *args_forms)

        if macrop(fun):
            form = fun[1](env, *args_forms)
            return eval(env, form)

        if not callablep(fun):
            raise Exception('first el %s of list %s is not callable' % (fun, form))

        return fun(*[eval(env, f) for f in args_forms])
    raise Exception('unknown form: %s' % form)
        

class Env:
    def __init__(self, parent=None, **kwargs):
        self.parent = parent
        self.d = {}
        self.d.update(kwargs)

    def __setitem__(self, k, v):
        self.d[k] = v

    def __getitem__(self, k):
        return self.d[k] if k in self.d else self.parent[k]

    def __contains__(self, k):
        return self.d.__contains__(k) or (self.parent and self.parent.__contains__(k))


def base_env():
    env = Env()
    env['t'] = True
    def list_(*args):
        return list(args)
    env['list'] = list_

    def dict_(**kwargs):
        return dict(**kwargs)
    env['dict'] = dict_

    def Tuple(*args):
        return tuple(args)
    env['Tuple'] = Tuple

    env['quote'] = special_form(lambda env, e: e)
    env['set'] = special_form(setq)

    env['def'] = special_form(defq)
    env['defun'] = special_form(defun)
    env['defmacro'] = special_form(defmacro)
    env['fn'] = special_form(fn)
    env['call'] = special_form(funcall)
    env['apply'] = special_form(apply)
    env['if'] = special_form(_if)

    env['+'] = operator.__add__
    env['-'] = operator.__sub__
    env['*'] = operator.__mul__
    env['/'] = operator.__truediv__

    def nth(i, l):
        return l[i]
    env['nth'] = nth

    def head(l):
        return l[0]
    env['head'] = head

    def tail(l):
        return l[1:]
    env['tail'] = tail
    
    env['make-struct'] = special_form(make_struct)
    return env


def progn(env, forms):
    r = None
    for form in forms:
        r = eval(env, form)
    return r


def interpret(forms):
    return progn(base_env(), forms)
    
