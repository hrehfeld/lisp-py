from .symbol import intern, Symbol


MACRO = '__macro'

def macrop(e):
    return isinstance(e, tuple) and len(e) == 2 and e[0] == MACRO


def Macro(f):
    if not callablep(f):
        raise Exception('%s is not callable' % (f))

    return (MACRO, f)


def eval_macro(m, args):
    return m[1](*args)
    

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

        if macrop(fun):
            return eval_macro(fun, args_forms)

        if not callablep(fun):
            raise Exception('first el %s of list %s is not callable' % (fun, form))

        return fun([eval(f, env) for f in args_forms])
        


def base_env():
    env = dict(
        t=True
        , list=list
        , quote=Macro(lambda e: e)
    )
    return env

def interpret(forms):
    env = base_env()
    r = None
    for form in forms:
        r = eval(form, env)
    return r
