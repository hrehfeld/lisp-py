from .symbol import intern, Symbol, symbol_name, symbolp
from .reader import read, Stream
import operator


MACRO = '__macro'
SPECIAL_FORM = '__special'

variadic_name = '&rest'
keys_name = '&keys'

class Struct:
    def __init__(self, name, fields, values):
        self.name = name
        assert(len(fields) == len(values))
        self.__slots__ = fields
        for k, v in zip(fields, values):
            setattr(self, k, v)

    def __eq__(self, o):
        if type(self) != type(o):
            return False
        for k in self.__slots__:
            if getattr(self, k) != getattr(o, k):
                return False
        return True


TYPE = '__type'
TYPE_T = '__type_t'


def Struct(name_str, *field_names):

    type = {TYPE: TYPE_T, 'name': name_str, 'fields': field_names}

    def constructor(*values):
        assert(len(fields) == len(values))
        r = {TYPE: type}
        r.update(zip(field_names, values))
        return r

    getters = []
    setters = []
    for ifield, field in enumerate(field_names):
        fname = '%s-%s' % (name_str, (field))
        get = lambda struct: struct[field]
        getters += [get]

        def set(struct, value):
            struct[field] = value
            return struct
        setters += [set]

    return constructor, getters, setters


def __defstruct(env, name, *fields):
    name_str = symbol_name(name)
    field_names = [symbol_name(f) for f in fields]

    constructor, getter, setter = Struct(name_str, field_names)
    env[name_str] = constructor
    for field, get in zip(field_names, getter):
        fname = '%s-%s' % (name_str, (field))
        env[fname] = get

    for field, set in zip(field_names, setter):
        fname = '%s-%s-set' % (name_str, (field))
        env[fname] = set

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

    def f(args, varargs, kwargs):
        fun_env = Env(env)
        for parameter, arg in zip(parameters, args):
            fun_env[symbol_name(parameter)] = arg

        varargs_name = special_names[variadic_name]
        if varargs_name:
            fun_env[symbol_name(varargs_name)] = varargs
        keysargs_name = special_names[keys_name]
        if keysargs_name:
            fun_env[symbol_name(keysargs_name)] = kwargs
        
        return __progn(fun_env, *body)
    add_function(f, parameters, defaults, special_names, special_defaults)
    return f


def __defun(env, name, parameters, *body):
    assert(symbolp(name))
    if symbol_name(name) in env:
        raise Exception('fun %s already declared' % symbol_name(name))

    f = __fn(env, parameters, *body)
    env[symbol_name(name)] = f
    return f


def __defmacro(lexical_env, name, parameters, *body):
    assert(symbolp(name)), (name, type(name))
    if symbol_name(name) in lexical_env:
        raise Exception('fun %s already declared' % symbol_name(name))

    f = __fn(lexical_env, parameters, *body)
    m = Macro(lambda dynamic_env, *args: __call(dynamic_env, f, args))
    lexical_env[symbol_name(name)] = m
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

    env = Env(env)

    for var in vars:
        name_sym, body = var
        val = __eval(env, body)
        env[symbol_name(name_sym)] = val
    return __progn(env, *let_body)


# TODO if without else
def __if(env, cond, then, *_else):
    cond = __eval(env, cond)
    # TODO check trueness test
    body = then
    if not cond and _else:
        body = _else[0]
    return __progn(env, body)


def is_num(f):
    return isinstance(f, int) or isinstance(f, float)


def is_str(f):
    return isinstance(f, str)


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


def __set_var(env, name, args):
    assert(len(args) <= 1), len(args)
    val = __eval(env, args[0]) if args else None
    env[symbol_name(name)] = val
    return val


def __def(env, name, *args):
    assert(symbolp(name))
    if symbol_name(name) in env:
        raise Exception('var %s already declared' % symbol_name(name))
    return __set_var(env, name, args)


def __setq(env, name, *args):
    assert(symbolp(name))
    if symbol_name(name) not in env:
        raise Exception('var %s not declared' % symbol_name(name))
    return __set_var(env, name, args)


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
            return -1

        kwargs = {}
        for iarg, arg in enumerate(args):
            # TODO we really need a type system to distinguish between symbols and tuples
            if isinstance(arg, tuple) and not symbolp(arg):
                k, v = arg
                i = parameter_index(k)
                if i == -1:
                    kwargs[k] = v
                else:
                    args_dict[i] = v
            else:
                v = arg
                i = iarg
                args_dict[i] = v

        for i, p in enumerate(parameters):
            if not (i in args_dict):
                raise Exception('function call missing argument #{i} {name}: ({fun} {args})'
                                .format(i=i, name=symbol_name(p), fun=fun, args=args_forms))

        if kwargs and special_names[keys_name] is None:
            raise Exception('unknown keyword args: {kws}'.format(kws=kwargs))

        args = []
        for i, p in enumerate(parameters):
            args += [args_dict[i]]

        num_args = len(args_dict)
        if num_args != len(parameters) and special_names[variadic_name] is None:
            raise Exception('too many arguments for call')

        varargs = []
        for i in range(len(parameters), num_args):
            varargs += [arg]

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
        form = __call_function(env, fun, [env] + args_forms, eval=False)
        return __eval(env, form)

    elif callablep(fun):
        return __call_function(env, fun, args_forms)

    else:
        raise Exception('({fun} {args}) is not callable'.format(fun=fun, args=args_forms if args_forms else ''))



def __eval(env, form):
    if is_num(form):
        return form
    elif is_str(form):
        return form
    elif keywordp(form):
        return form
    elif symbolp(form):
        if not symbol_name(form) in env:
            raise Exception('Symbol "%s" not found in env (Keys: %s)' % (symbol_name(form), ', '.join(env.d.keys())))
        return env[symbol_name(form)]
    elif listp(form):
        if not length(form):
            raise Exception('trying to evaluate list of length 0')
        fun = __eval(env, form[0])
        args_forms = form[1:]
        
        return __call(env, fun, args_forms)
    else:
        raise Exception('unknown form: %s' % form)
        

class Env:
    def __init__(self, parent=None):
        self.parent = parent
        self.d = {}

    def __setitem__(self, k, v):
        self.d[k] = v

    def __getitem__(self, k):
        return self.d[k] if k in self.d else self.parent[k]

    def __contains__(self, k):
        return self.d.__contains__(k) or (self.parent and self.parent.__contains__(k))


def base_env(args=[]):
    env = Env()
    env['true'] = True
    env['false'] = False
    env['nil'] = None
    def list_(*args):
        return list(args)
    env['list'] = list_

    env['dict'] = dict
    env['dict-setdefault'] = dict.setdefault
    env['dict-get'] = dict.get
    def dict_set(d, k, v):
        d[k] = v
    env['dict-set'] = dict_set

    def Tuple(*args):
        return tuple(args)
    env['tuple'] = Tuple

    def __import(env, *args):
        # TODO 
        pass
    env['import'] = special_form(__import)

    def __while(env, cond, *body):
        while __eval(env, cond):
            __eval(env, [intern('__progn'), *body])

    env['while'] = special_form(__while)

    def __lookup(env, obj, *ks):
        r = __eval(env, obj)
        for k in ks:
            r = getattr(r, symbol_name(k))
        return r

    env['.'] = special_form(__lookup)

    env['quote'] = special_form(lambda env, e: e)
    env['set'] = special_form(__setq)
    env['let'] = special_form(__let)
    env['progn'] = special_form(__progn)

    env['def'] = special_form(__def)
    env['defun'] = special_form(__defun)
    env['defmacro'] = special_form(__defmacro)
    env['fn'] = special_form(__fn)
    env['call'] = special_form(__funcall)
    env['apply'] = special_form(__apply)
    env['if'] = special_form(__if)

    env['+'] = operator.__add__
    env['-'] = operator.__sub__
    env['*'] = operator.__mul__
    env['/'] = operator.__truediv__

    env['eq'] = operator.__eq__
    env['neq'] = operator.__ne__

    env['not'] = operator.__not__
    env['and'] = operator.__and__
    env['or'] = operator.__or__

    env['<'] = operator.__lt__
    env['<='] = operator.__le__
    env['>'] = operator.__gt__
    env['>='] = operator.__ge__

    def length(l):
        return len(l)
    env['length'] = length

    def has(l, e):
        return e in l
    env['contains?'] = has

    def nth(i, l):
        return l[i]
    env['nth'] = nth

    def head(l):
        return l[0]
    env['head'] = head

    def tail(l):
        return l[1:]
    env['tail'] = tail
    
    env['defstruct'] = special_form(__defstruct)


    def throw(e):
        raise e
    env['throw'] = throw

    def exception(s):
        return Exception(s)
    env['Exception'] = exception
    
    # sys utils
    def file_open(filename, mode):
        assert(symbolp(mode))
        mode = symbol_name(mode)
        return open(filename, mode)

    env['file-open'] = file_open
    
    env['argv'] = args

    import pathlib
    env['make-Path'] = pathlib.Path

    def print_(*args):
        s = ' '.join(map(str(args)))
        print(s)
        return s

    env['print'] = print_

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
    
