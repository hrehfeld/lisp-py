def native(f):
    return f

TYPE = '__type'
TYPE_T = '__type_t'

def make_dict(*args):
    assert (len(args) % 2 == 0), args
    kwargs = {}
    for i in range(0, len(args), 2):
        k = args[i]
        v = args[i + 1]
        kwargs[k] = v
    return kwargs
        

def defstruct(name_str, *field_names):
    for n in field_names:
        assert(isinstance(n, str))
    type = make_dict(TYPE, TYPE_T, 'name', name_str, 'fields', field_names)

    def constructor(*values):
        assert(len(field_names) == len(values))
        r = {TYPE: type}
        for k, v in zip(field_names, values):
            r[k] = v
        return r

    def instancep(obj):
        return isinstance(obj, dict) and obj.get(TYPE, None) == type

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

    return constructor, instancep, getters, setters


def is_struct(obj):
    return isinstance(obj, dict) and TYPE in obj

def concat(*rest):
    r = ''
    for e in rest:
        r += e
    return r
