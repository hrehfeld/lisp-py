def eval(form):
    pass


def interpret(forms):
    r = None
    for form in forms:
        r = eval(form)
    return r
