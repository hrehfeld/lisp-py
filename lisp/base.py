def native(f):
    return f

TYPE = '__type'
TYPE_T = '__type_t'


def Struct(name_str, *field_names):
    for n in field_names:
        assert(isinstance(n, str))
    type = {TYPE: TYPE_T, 'name': name_str, 'fields': field_names}

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


def concat(start, *rest):
    for e in rest:
        start += e
    return start
