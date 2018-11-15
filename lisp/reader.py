from .symbol import intern

whitespace = ' \t\n'
newlines = '\n' #TODO add windows mac shit
token_end_chars = whitespace + ')'

floating_point = '.'
str_start = '"'
str_end = '"'

accessor_char = "."
quote_char = "'"
comment_chars = ";", 


class Valid:
    def __init__(self, expr):
        self.expr = expr


class Return(Valid):
    pass


def ends_token(s):
    """assumes that all tokens are ended by one of token_end_chars"""
    return stream_peek(s) in token_end_chars


def is_paren_open(c):
    return c == '('


def is_paren_close(c):
    return c == ')'


def is_whitespace(c):
    return c in " \n\t"


class Stream:
    def __init__(self, program, i):
        self.program = program
        self.i = i


def stream_pos(self):
    return self.i


def stream_token(self, a, b):
    return self.program[a:b]


def stream_peek(self):
    assert isinstance(self, Stream)
    return self.program[self.i]


def stream_next(self):
    assert isinstance(self, Stream)
    c = self.program[self.i]
    self.i += 1
    return c


def stream_advance(self, n):
    assert isinstance(self, Stream), str(type(self))
    self.i += n


def stream_empty(self):
    assert isinstance(self, Stream), str(type(self))
    return self.i >= len(self.program)


def next_token_is(reader, s):
    return reader(s) != 0


def make_parse_fail(msg):
    def parse_fail(token):
        raise Exception('unexpected %s: %s' % (token, msg))
    return parse_fail


def read_list(s):
    if stream_empty(s) or not is_paren_open(stream_next(s)):
        return
    # TODO check if ) throws error
    def read_list_end(s):
        if stream_empty(s) or not is_paren_close(stream_next(s)):
            return None
        return Return(None)

    els = read(s, readers=[read_list_end] + readers)
    return Valid(els)


def _read_int(s):
    istart = s.i
    unary = 0
    if not stream_empty(s) and stream_peek(s) in ('-', '+'):
        stream_next(s)
        unary = 1
    while not stream_empty(s) and stream_peek(s) in '0123456789':
        stream_next(s)
    return istart + unary < s.i
        

def read_num(s):
    istart = stream_pos(s)
    parsed = _read_int(s)
    num = int
    if not stream_empty(s) and stream_peek(s) in floating_point:
        stream_next(s)
        parsed = _read_int(s)
        num = float
    parsed = parsed and (stream_empty(s) or ends_token(s))
    if not parsed:
        return None
    s = stream_token(s, istart, stream_pos(s))
    return Valid(num(s))


def read_str(s):
    istart = stream_pos(s)
    if not stream_peek(s) in str_start:
        return None
    stream_next(s)
    while not stream_empty(s) and stream_next(s) not in str_end:
        pass
    if stream_empty(s):
        return None
    return Valid(stream_token(s, istart, stream_pos(s)))


def read_whitespace(s):
    parsed = None
    while not stream_empty(s) and stream_peek(s) in whitespace:
        stream_next(s)
        parsed = True
    return Valid(None) if parsed else None


def read_comment(s):
    if stream_next(s) not in comment_chars:
        return None
    while not stream_empty(s) and stream_peek(s) not in newlines:
        stream_next(s)
    return Valid(None)


def read_symbol(s):
    istart = stream_pos(s)
    parsed = None
    while not (stream_empty(s) or ends_token(s)):
        stream_next(s)
        parsed = True
    if not parsed:
        return None

    token = stream_token(s, istart, stream_pos(s))
    if accessor_char not in token:
        return Valid(intern(token))
    else:
        accessors = token.split(accessor_char)
        return Valid([intern(s) for s in [accessor_char, *accessors]])
        

def read_quote(s):
    if stream_next(s) != quote_char:
        return None
    
    # quote only supports one following exp
    expr =  read(s, one=True)

    r = [intern('quote'), *expr]
    return Valid(r)
    

readers = [
    (read_list)
    , (read_whitespace)
    , (read_comment)
    , (read_num)
    , (read_str)
    , (read_quote)
    , (read_symbol)
]


def valid_action(a):
    return isinstance(a, Valid)


def return_action(a):
    return isinstance(a, Return)
    

def read(s, readers=readers, one=False):
    assert isinstance(s, Stream), type(s)
    r = []
    res = None
    while not stream_empty(s) and not return_action(res):
        for reader in readers:
            istart = s.i
            res = reader(s)
            if valid_action(res):
                e = res.expr
                if e is not None:
                    r.append(e)
                break
            else:
                s.i = istart
        if not valid_action(res):
            raise Exception('Unexpected: "%s" at %s' % (stream_peek(s), s.i))
        if one:
            break
    return r

