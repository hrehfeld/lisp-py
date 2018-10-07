from .symbol import intern, Symbol


def is_num(f):
    return isinstance(f, int) or isinstance(f, float)


def symbolp(e):
    return isinstance(e, Symbol)

def eval(form, env):
    if is_num(form):
        return form
    if symbolp(form):
        if not form.s in env:
            raise Exception('Symbol %s not found in env (Keys: %s)' % (form.s, ', '.join(env.keys())))
        return env[form.s]
        


def base_env():
    env = dict(
        t=True
    )
    return env

def interpret(forms):
    env = base_env()
    r = None
    for form in forms:
        r = eval(form, env)
    return r
