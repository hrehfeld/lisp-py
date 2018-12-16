from .base import native, defstruct, is_struct, TYPE
from .base import keyword, keyword_name, is_keyword, special_form, is_special_form, is_list, is_num, is_str, is_int, is_atom, is_callable, length, special_form_get_fun, is_special_keyword, Macro, is_macro, macro_get_fun
from .base import make_env, env_contains, env_get, env_def, env_change, env_d, env_parent
from .base import sexps_str, ps, is_named_operator
from .symbol import intern, symbol_name, is_symbol, gensym
from .reader import read, Stream, quote_fun_name, backquote_fun_name, backquote_eval_fun_name, backquote_splice_fun_name, quote_char, backquote_char, backquote_eval_char, backquote_splice_char, keyword_start
import operator

@native
class BlockException(Exception):
    def __init__(self, name, value=None):
        Exception.__init__(self, name)
        self.name = name
        self.value = value


variadic_name = '&rest'
keys_name = '&keys'
nokeys_name = '&nokeys'


def __defstruct(env, name, *fields):
    # FIXME: for bootstrapping
    if is_str(name):
        name = intern(name)
    fields = [intern(f) if is_str(f) else f for f in fields]

    assert(is_symbol(name)), name
    name_str = symbol_name(name)
    field_names = [symbol_name(f) for f in fields]

    constructor, is_instance, getter, setter = defstruct(name_str, *field_names)
    env_def(env, name_str, constructor)
    fname = '%s?' % (name_str)
    env_def(env, fname, is_instance)
    for field, get in zip(field_names, getter):
        fname = '%s-%s' % (name_str, (field))
        env_def(env, fname, get)

    for field, set in zip(field_names, setter):
        fname = '%s-%s-set' % (name_str, (field))
        env_def(env, fname, set)

    return constructor


functions = {}


def add_function(f, *args):
    functions[f] = args


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
    

def parameter_default(p):
    return p[1]


def is_parameter_with_default_(p):
        return isinstance(p, list) and len(p) == 2 and is_keyword(p[0])


def is_simple_parameter(p):
    return is_symbol(p) and not is_keyword(p) and not is_special_keyword(p)


def is_normal_parameter(p):
    return is_simple_parameter(p) or is_parameter_with_default_(p)


def normal_parameter_name(p):
    if is_parameter_with_default_(p):
        return p[1]
    else:
        return p


def __fn(env, parameters, *body):

    specials = {variadic_name, keys_name, nokeys_name}
    special_allows_next = {variadic_name, keys_name}
    # TODO support start_only specials
    special_end_only = {variadic_name, keys_name}
    special_once_only = {variadic_name, keys_name, nokeys_name}

    parsed_parameters = []
    special_used = set()

    special_params = {variadic_name: None, keys_name: None}

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
                raise Exception('Unkown special keyword: {s} at position {i}'.format(s=p, i=i))
            if param_special in special_params:
                special_params[param_special] = True
            if param_special in special_allows_next:
                if has_normal_next:
                    # skip next parameter which we just parsed
                    i += 1
                    if param_special in special_params:
                        special_params[param_special] = next_param
                
            if param_special in special_once_only and param_special in special_used:
                raise Exception('{special} parameter defined more than once'.format(special=param_special))
            if param_special in special_end_only:
                end = True
            elif end:
                raise Exception('{special} parameters must be defined after normal ones'.format(special=param_special))

            if param_special is not None:
                special_used.add(param_special)

        if param:
            param_name = normal_parameter_name(param)
            if is_parameter_with_default_(param):
                param_default = parameter_default(param)
        if param_name:
            if symbol_name(param_name) in used_names:
                raise Exception('Duplicate parameter {name}'.format(name=n))
            used_names.add(symbol_name(param_name))
            
            parsed_parameters.append((param_name, param_default))
        i += 1

    block_name = gensym()
    def f(args, varargs, kwargs):
        fun_env = make_env(env)
        env_def(fun_env, 'return', lambda value=None: return_from(fun_env, block_name, value))

        for (parameter, default), arg in zip(parsed_parameters, args):
            assert(is_symbol(parameter)), parameter
            env_def(fun_env, symbol_name(parameter), arg)

        varargs_name = special_params[variadic_name]
        if is_symbol(varargs_name):
            env_def(fun_env, symbol_name(varargs_name), varargs)
        keysargs_name = special_params[keys_name]
        if is_symbol(keysargs_name):
            env_def(fun_env, symbol_name(keysargs_name), kwargs)

        return block(fun_env, block_name, *body)
    #print('&&&&&&&&', special_used, sexps_str(parameters), nokeys_name in special_used)
    add_function(f, parsed_parameters
                 , nokeys_name in special_used
                 , variadic_name in special_used
                 , keys_name in special_used
    )
    return f


def __defun(env, name, parameters, *body):
    assert(is_symbol(name))
    #if env_contains(env, symbol_name(name)):
    #    raise Exception('fun %s already declared' % symbol_name(name))

    f = __fn(env, parameters, *body)
    env_def(env, symbol_name(name), f)
    return f


def __defmacro(lexical_env, name, parameters, *body):
    assert(is_symbol(name)), (name, type(name))
    if env_contains(lexical_env, symbol_name(name)):
        raise Exception('fun %s already declared' % symbol_name(name))

    f = __fn(lexical_env, parameters, *body)
    m = Macro(f)
    env_def(lexical_env, symbol_name(name), m)
    return m


def __funcall(env, f, *args):
    f = __eval(env, f)
    return __call(env, f, args, do_eval_args=True)
    

def __apply(env, f, args):
    f = __eval(env, f)
    args = __eval(env, args)
    return __call(env, f, args, do_eval_args=False)
    

def __sub_env(env, *body):
    sub_env = make_env(env)
    return __progn(sub_env, *body)

def __let(env, vars, *let_body):
    for var in vars:
        assert(is_list(var)), sexps_str(var)
        assert(len(var) == 2)
        assert (is_symbol(var[0])), sexps_str(var)

    let_env = make_env(env)

    for var in vars:
        name_sym, body = var
        val = __eval(let_env, body)
        env_def(let_env, symbol_name(name_sym), val)
    return __progn(let_env, *let_body)


# TODO if without else
def __if(env, cond, then, *else_body):
    cond = __eval(env, cond)
    if cond:
        r = __eval(env, then)
    elif else_body:
        r = __progn(env, *else_body)
    else:
        r = None
    return r


def __def(env, name, *args):
    assert(is_symbol(name))
    if env_contains(env, symbol_name(name)):
        raise Exception('var %s already declared' % symbol_name(name))
    val = __eval(env, args[0]) if args else None
    env_def(env, symbol_name(name), val)
    return val


def __setq(env, name, value):
    assert(env is not None)
    assert(is_symbol(name))
    #if not env_contains(env, symbol_name(name)):
    #    raise Exception('set: {sym} not declared in {env} ({is_env})'
    #                    .format(sym=symbol_name(name), env=sexps_str(env_d(env)), is_env=sexps_str(env_d(env_parent(env)) if env_parent(env) else '{}'))
    value = __eval(env, value)
    env_change(env, symbol_name(name), value)
    return value


def __call_function(env, fun, args_forms, eval=True):
    if fun not in functions:
        # python fun
        # we don't support named normal args -- would need reflection

        kw = None
        args = []
        varargs = []
        ilast_normal_arg = -1
        for iarg, arg in enumerate(args_forms):
            is_last_arg = iarg + 1 >= len(args_forms)
            # FIXME: support ((fn (a b) (list a b)) :a 0) => '(:a 0)
            if is_keyword(arg) and not (is_last_arg or kw):
                kw = arg
            else:
                k = iarg
                if kw:
                    # without :
                    k = symbol_name(kw)[1:]
                    kw = None
                else:
                    ilast_normal_arg = iarg
                args += [(k, arg)]
        del kw

        clean_args = []
        kwargs = {}
        for iarg, arg in enumerate(args):
            if iarg > ilast_normal_arg:
                k, v = arg
                kwargs[k] = v
            else:
                # TODO hack: we really need to fix our type system
                if isinstance(arg, tuple) and not is_symbol(arg):
                    k, v = arg
                else:
                    v = arg
                clean_args += [v]

        args = clean_args
        del clean_args

        return fun(*args, **kwargs)
    else:
        # self-defined fun
        (parameters, nokeys, set_varargs, set_kwargs) = functions[fun]

        #(param_name, param_default, param_special) = parameters[iparam]
        iparam = 0
        args = []
        kwargs = {}

        remaining_args = args_forms
        keywords_started = False
        while remaining_args:
            arg = remaining_args.pop(0)

            if is_keyword(arg) and remaining_args and not nokeys:
                keywords_started = True
                key = keyword_name(arg)
                arg = remaining_args.pop(0)
                kwargs[key] = arg
            else:
                if keywords_started:
                    raise Exception('positional argument follows keyword argument')

                if not set_varargs and len(args) >= len(parameters):
                    raise Exception('too many arguments for function call: ({fun} {args})'
                                        .format(name=param_name, fun=fun, args=sexps_str(args_forms)))
                
                args.append(arg)
            del arg

        if len(args) < len(parameters) and kwargs:
            for i, p in enumerate(parameters[len(args):]):
                assert(isinstance(p, tuple)), p
                (param_name, param_default) = p
                n = symbol_name(param_name)
                if n not in kwargs:
                    raise Exception('function call missing argument {name}: ({fun} {args})'
                                        .format(name=n, fun=fun, args=sexps_str(args_forms)))
                args.append(kwargs[n])
                del kwargs[n]
                    
        if len(parameters) > len(args):
            raise Exception('function call missing argument #{i}: ({fun} {args})'
                                .format(i=len(args), fun=fun, args=sexps_str(args_forms)))

        if not set_kwargs and kwargs:
            raise Exception('Unexpected keyword arguments for function call: ({fun} {args})'
                            .format(fun=fun, args=sexps_str(args_forms)))

        varargs = []
        if set_varargs and len(args) > len(parameters):
            varargs = args[len(parameters):]
            args = args[:len(parameters)]
        return fun(args, varargs, kwargs)
    

def __call(env, fun, args_forms, do_eval_args):
    if is_special_form(fun):
        fun = special_form_get_fun(fun)
        return __call_function(env, fun, [env] + args_forms, eval=False)

    elif is_macro(fun):
        fun = macro_get_fun(fun)
        form = __call_function(env, fun, args_forms, eval=False)
        return __eval(env, form)

    elif is_callable(fun):
        if do_eval_args:
            args_forms = [__eval(env, arg) for arg in args_forms]
        return __call_function(env, fun, args_forms)

    else:
        raise Exception('({fun} {args}) is not callable'.format(fun=fun, args=sexps_str(args_forms) if args_forms else ''))


def __eval(env, form):
    if is_symbol(form) and not is_keyword(form):
        if not env_contains(env, symbol_name(form)):
            raise Exception('Symbol "{sym}" not found in env \nKeys: {keys}\nParent keys: {pkeys}'
                            .format(sym=symbol_name(form)
                                    , keys=', '.join(sorted(env_d(env).keys()))
                                    , pkeys=', '.join(map(str, sorted(env_d(env_parent(env)).keys()))) if env_parent(env) else ''))
        return env_get(env, symbol_name(form))
    elif is_atom(form):
        return form
    elif is_list(form):
        if not length(form):
            raise Exception('trying to evaluate list of length 0')
        fun = __eval(env, form[0])
        args_forms = form[1:]
        
        return __call(env, fun, args_forms, do_eval_args=True)
    else:
        raise Exception('unknown form: {form}'.format(form=sexps_str(form)))
        

def base_env(args=[]):
    env = make_env()

    env_def(env, 'true', True)
    env_def(env, 'false', False)
    env_def(env, 'nil', None)
    def list_(*args):
        return list(args)
    env_def(env, 'list', list_)

    def as_list(arg):
        return list(arg)
    env_def(env, 'as-list', as_list)

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
    env_def(env, 'repr', lambda arg: sexps_str(arg))

    env_def(env, 'intern', intern)
    env_def(env, 'symbol-name', symbol_name)

    env_def(env, 'keyword', keyword)
    env_def(env, 'keyword-name', keyword_name)

    env_def(env, 'dict', dict)
    env_def(env, 'dict-setdefault', dict.setdefault)

    @native
    def dict_set(d, **kwargs):
        for k, v in kwargs.items():
            d[k] = v
    env_def(env, 'dict-set', dict_set)
    env_def(env, 'dict_set', dict_set)

    env_def(env, 'aref', lambda l, k: l[k])

    def list_set(l, k, v):
        assert(is_list(l))
        assert(is_int(k))
        l[k] = v
        return v
    env_def(env, 'list-set', list_set)
    
    def Tuple(*args):
        return tuple(args)
    env_def(env, 'tuple', Tuple)


    def __assert(env, cond, msg=''):
        r = __eval(env, cond)
        if not r:
            msg = __eval(env, msg)
            msg = '%s: %s' % (sexps_str(cond), msg)
            
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

    def is_backquote_eval(s):
        return is_named_operator(s, intern(backquote_eval_fun_name)) or is_named_operator(s, intern(backquote_splice_fun_name))

    backquote_level_var = '*__backquote_level*'

    def backquote_(env, s):
        if is_atom(s):
            return [s]
        elif is_named_operator(s, intern(backquote_eval_fun_name)):
            arg = s[1]
            assert(len(s) == 2)
            #print('~:', level, sexps_str(s))

            form = s[1]
            nested_level = 0
            while is_named_operator(form, intern(backquote_eval_fun_name)):
                nested_level += 1
                form = form[1]
            if is_named_operator(form, intern(backquote_splice_fun_name)):
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
        elif is_named_operator(s, intern(backquote_splice_fun_name)):
            #print('~@:', level, sexps_str(s))
            assert(len(s) == 2)
            form = s[1]
            r = __eval(env, form)
            #print('~@:', level, sexps_str(r))
            assert(is_list(r)), (r, sexps_str(s))
            return r
        elif is_named_operator(s, intern(backquote_fun_name)):
            #print('`:', level, sexps_str(s))
            assert(len(s) == 2)
            form = s[1]
            r = backquote_(env, form)
            assert(len(r) == 1)
            r = [[intern(backquote_fun_name)] + r]
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
            raise Exception(sexps_str(s))

    def backquote(env, s):
        #print('backquote:', level, sexps_str(s))
        r = backquote_(env, s)
        assert(len(r) == 1)
        r = r[0]
        #print('backquote:', level, sexps_str(r))
        return r

    env_def(env, backquote_fun_name, special_form(backquote))

    def source_eval(env, form):
        form = __eval(env, form)
        return __eval(env, form)
    env_def(env, 'eval', special_form(source_eval))
    env_def(env, 'set', special_form(__setq))
    env_def(env, '__sub-env', special_form(__sub_env))
    env_def(env, 'let*', special_form(__let))
    env_def(env, 'progn', special_form(__progn))

    env_def(env, 'def', special_form(__def))
    env_def(env, 'defun', special_form(__defun))
    env_def(env, 'defmacro', special_form(__defmacro))
    env_def(env, 'fn', special_form(__fn))
    env_def(env, 'call', special_form(__funcall))
    env_def(env, 'apply', special_form(__apply))
    env_def(env, 'if', special_form(__if))

    env_def(env, 'gensym', special_form(lambda env, *args: gensym(*args)))

    env_def(env, 'null?', lambda *args: all([e is None for e in args]))

    env_def(env, 'symbolp', is_symbol)
    env_def(env, 'symbol?', is_symbol)

    env_def(env, 'keyword?', is_keyword)

    env_def(env, 'listp', is_list)
    env_def(env, 'list?', is_list)

    env_def(env, 'num?', is_num)

    env_def(env, 'dict?', lambda d: isinstance(d, dict) and not is_struct(d))

    def is_tuple(v):
        return isinstance(v, tuple)
    env_def(env, 'tuplep', is_tuple)
    env_def(env, 'tuple?', is_tuple)

    env_def(env, 'str?', is_str)

    env_def(env, 'int?', is_int)

    env_def(env, 'named-operator?', is_named_operator)

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
    env_def(env, 'mod', numeric_op(operator.__mod__))

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
        assert is_list(l), repr(l)
        return [e] + l
    env_def(env, 'cons', cons)

    def _slice(l, istart=None, *args):
        n = len(args)
        assert(n <= 2)
        if n < 2:
            if n == 0:
                iend = istart
                istart = 0
            else:
                iend = args[0]

            if iend is None:
                iend = len(l)
            return l[istart:iend]
        else:
            if istart is None:
                istart = 0
            iend, step = args
            if iend is None:
                iend = len(l)
            if step is None:
                step = 1
            return [l[i] for i in range(istart, iend, step)]
    env_def(env, 'slice', _slice)


    @native
    def length(l):
        return len(l)
    env_def(env, 'length', length)

    def has(l, e):
        return e in l
    env_def(env, 'contains?', has)

    def nth(i, l):
        assert (is_list(l) or is_tuple(l)),  sexps_str(l)
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
        assert(is_symbol(mode))
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

    env = make_env(env)
    with open('stdlib.lisp', 'r') as f:
        interpret(read(Stream(f.read(), 0)), env)
    env = make_env(env)
    return env


def __progn(env, *forms):
    r = None
    for form in forms:
        r = __eval(env, form)
    return r


def interpret(forms, env=None, args=[]):
    if env is None:
        env = base_env(args)
    env_def(env, '__name__', '<self>')
    return __progn(env, *forms)
    
