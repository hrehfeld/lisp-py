from .symbol import intern, Symbol

import operator


MACRO = '__macro'
SPECIAL_FORM = '__special'


def special_formp(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == SPECIAL_FORM


def special_form(f):
    if not callablep(f):
        raise Exception('%s is not callable' % (f))
    return (SPECIAL_FORM, f)


def eval_special_form(env, m, args):
    return m[1](env, *args)
    

def macrop(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == MACRO


def Macro(f):
    if not callablep(f):
        raise Exception('%s is not callable' % (f))

    return (MACRO, f)


def eval_macro(env, m, args):
    form = m[1](env, *args)
    return eval(form, env)
    

def eval_fun(f, args):
    assert(isinstance(args, list))
    assert(callable(f))
    print(f, args, type(args))
    return f(*args)
    

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
    val = eval(args[0], env) if args else None
    env[name.s] = val
    return val


def defq(env, name, *args):
    assert(symbolp(name))
    if name.s in env:
        raise Exception('var %s already declared' % name.s)
    return set_var(env, name, args)


def setq(env, name, *args):
    assert(symbolp(name))
    if name.s not in env:
        raise Exception('var %s not declared' % name.s)
    return set_var(env, name, args)


def eval(form, env):
    if is_num(form):
        return form
    if symbolp(form):
        if not form.s in env:
            raise Exception('Symbol %s not found in env (Keys: %s)' % (form.s, ', '.join(env.keys())))
        return env[form.s]
    if listp(form):
        if not length(form):
            raise Exception('trying to evaluate list of length 0')
        fun = eval(form[0], env)
        args_forms = form[1:]

        if special_formp(fun):
            return eval_special_form(env, fun, args_forms)

        if macrop(fun):
            return eval_macro(env, fun, args_forms)

        if not callablep(fun):
            raise Exception('first el %s of list %s is not callable' % (fun, form))

        return eval_fun(fun, [eval(f, env) for f in args_forms])
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
    env = Env(
        t=True
        , list=lambda *args: list(args)
        , quote=special_form(lambda env, e: e)
        , set=special_form(setq)
    )
    env['def'] = special_form(defq)

    env['+'] = operator.__add__
    env['-'] = operator.__sub__
    env['*'] = operator.__mul__
    env['/'] = operator.__truediv__
    return env


def progn(env, forms):
    r = None
    for form in forms:
        r = eval(form, env)
    return r


def interpret(forms):
    return progn(base_env(), forms)
    
