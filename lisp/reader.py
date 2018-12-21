from .symbol import intern

whitespace = ' \t\n'
newlines = '\n' #TODO add windows mac shit
token_end_chars = whitespace + ')'

keyword_start = ':'
floating_point = '.'
str_start = '"'
str_end = '"'

accessor_char = "."
quote_char = "'"
backquote_char = "`"
# clojur style
backquote_eval_char = "~"
backquote_splice_char = "~@"
comment_chars = ";", 
escape_chars = '\\'

special_chars = dict(n='\n', t='\t')


quote_fun_name = "quote"
backquote_fun_name = "backquote"
backquote_eval_fun_name = "unquote"
backquote_splice_fun_name = "unquote-splice"


VALID = '__VALID'
RETURN = '__RETURN'

def Valid(expr):
    return (VALID, expr)


def Return(expr):
    return (RETURN, expr)


def valid_action(a):
    return isinstance(a, tuple) and len(a) == 2 and a[0] in (VALID, RETURN)


def return_action(a):
    return isinstance(a, tuple) and len(a) == 2 and a[0] == RETURN
    

def get_expr(s):
    return s[1]


def ends_token(s):
    """assumes that all tokens are ended by one of token_end_chars"""
    return stream_peek(s) in token_end_chars


def is_paren_open(c):
    return c == '('


def is_paren_close(c):
    return c == ')'


def is_whitespace(c):
    return c in " \n\t"


def Stream(source, pos):
    return dict(source=source, pos=pos)


def stream_source(self):
    return self['source']


def stream_pos(self):
    return self['pos']


def stream_pos_set(self, pos):
    self['pos'] = pos


def stream_token(self, a, b):
    return stream_source(self)[a:b]


def stream_peek(self):
    return stream_source(self)[stream_pos(self)]


def stream_next(self):
    c = stream_source(self)[stream_pos(self)]
    stream_pos_set(self, stream_pos(self) + 1)
    return c


def stream_advance(self, n):
    stream_pos_set(self, stream_pos(self) + n)


def stream_empty(self):
    return stream_pos(self) >= len(stream_source(self))


def next_token_is(reader, s):
    return reader(s) != 0


def make_parse_fail(msg):
    def parse_fail(token):
        raise Exception('unexpected %s: %s' % (token, msg))
    return parse_fail


def read_list(s):
    if stream_empty(s) or not is_paren_open(stream_next(s)):
        return
    else:
        # TODO check if ) throws error
        def read_list_end(s):
            if stream_empty(s) or not is_paren_close(stream_next(s)):
                return None
            return Return(None)

        els = read(s, readers=[read_list_end] + readers)
        return Valid(els)


def _read_int(s):
    istart = stream_pos(s)
    unary = 0
    if not stream_empty(s) and stream_peek(s) in ('-', '+'):
        stream_next(s)
        unary = 1
    while not stream_empty(s) and stream_peek(s) in '0123456789':
        stream_next(s)
    return istart + unary < stream_pos(s)
        

def read_num(s):
    istart = stream_pos(s)
    parsed = _read_int(s)
    num = int
    if not stream_empty(s) and stream_peek(s) in floating_point:
        stream_next(s)
        parsed = _read_int(s)
        num = float
    parsed = parsed and (stream_empty(s) or ends_token(s))
    if parsed:
        s = stream_token(s, istart, stream_pos(s))
        return Valid(num(s))
    return None


def read_str(s):
    if stream_peek(s) in str_start:
        stream_next(s)
        r = ''
        escape_open = False
        while not stream_empty(s) and (stream_peek(s) not in str_end or escape_open):
            c = stream_next(s)
            if c in escape_chars:
                if escape_open:
                    r += c
                escape_open = not escape_open
            else:
                if escape_open and c in special_chars:
                    c = special_chars[c]
                r += c
                escape_open = False
        if not stream_empty(s):
            stream_next(s)
            return Valid(r)
    return None


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
    if parsed:
        token = stream_token(s, istart, stream_pos(s))
        if accessor_char not in token:
            return Valid(intern(token))
        else:
            if token == accessor_char:
                return Valid(intern(token))
            accessors = token.split(accessor_char)
            accessors = [a for a in accessors if a]
            
            return Valid([intern(s) for s in [accessor_char, *accessors]])
    return None
        

def read_quote_like(s, quote_char, symstr):
    for c in quote_char:
        if stream_next(s) != c:
            return None
    # quote only supports one following exp
    expr =  read(s, one=True)
    
    r = [intern(symstr), *expr]
    return Valid(r)


def read_quote(s):
    return read_quote_like(s, quote_char, quote_fun_name)
    

def read_backquote(s):
    return read_quote_like(s, backquote_char, backquote_fun_name)
    

def read_backquote_eval(s):
    return read_quote_like(s, backquote_eval_char, backquote_eval_fun_name)
    

def read_backquote_splice(s):
    return read_quote_like(s, backquote_splice_char, backquote_splice_fun_name)
    

readers = [
    (read_list)
    , (read_whitespace)
    , (read_comment)
    , (read_num)
    , (read_str)
    , (read_backquote_splice)
    , (read_backquote_eval)
    , (read_quote)
    , (read_backquote)
    , (read_symbol)
]


def read(s, readers=readers, one=False):
    r = []
    res = None
    while not stream_empty(s) and not return_action(res):
        for reader in readers:
            istart = stream_pos(s)
            res = reader(s)
            if valid_action(res):
                e = get_expr(res)
                if e is not None:
                    r.append(e)
                break
            else:
                stream_pos_set(s, istart)
        if not valid_action(res):
            raise Exception('Unexpected: "%s" at %s' % (stream_peek(s), stream_pos(s)))
        # one reads until one actual token is parsed
        elif r and one:
            break
    return r

