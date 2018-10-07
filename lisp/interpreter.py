from .symbol import intern


def is_num(f):
    return isinstance(f, int) or isinstance(f, float)


def eval(form):
    if is_num(form):
        return form


def interpret(forms):
    r = None
    for form in forms:
        r = eval(form)
    return r
