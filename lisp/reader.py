from .symbol import intern

whitespace = ' \t\n'
token_end_chars = whitespace + ')'

floating_point = '.'

quote_char = "'"


def ends_token(s):
    """assumes that all tokens are ended by one of token_end_chars"""
    return s.peek() in token_end_chars


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

    def peek(self):
        return self.program[self.i]

    def next(self):
        c = self.program[self.i]
        self.i += 1
        return c

    def advance(self, n):
        self.i += n

    def empty(self):
        return self.i >= len(self.program)


def next_token_is(reader, s):
    return reader(s) != 0


def make_parse_fail(msg):
    def parse_fail(token):
        raise Exception('unexpected %s: %s' % (token, msg))
    return parse_fail


def read_list(s):
    if s.empty() or not is_paren_open(s.next()):
        return None
    read_sublist(s)
    return True


def read_sublist(s):
    # TODO check if ) throws error
    def read_list_end(s):
        if not s.empty() and is_paren_close(s.next()):
            return True

    def parse_list_end(token):
        return None, RETURN_ACTION

    els = read(s, readers=[(read_list_end, parse_list_end)] + readers_parsers)
    return els


def parse_list(token):
    els = read_sublist(Stream(token, 1))
    return els, None


def _read_int(s):
    istart = s.i
    while not s.empty() and s.peek() in '0123456789':
        s.next()
    return istart != s.i
        

def read_num(s):
    istart = s.i
    _read_int(s)
    if not s.empty() and s.peek() in floating_point:
        s.next()
        _read_int(s)
    return istart != s.i and (s.empty() or ends_token(s))


def parse_num(token):
    num = float if floating_point in token else int
    return num(token), None
    


def read_whitespace(s):
    parsed = None
    while not s.empty() and s.peek() in whitespace:
        s.next()
        parsed = True
    return parsed


def parse_whitespace(token):
    return None, None
        

def read_symbol(s):
    parsed = None
    while not (s.empty() or ends_token(s)):
        s.next()
        parsed = True
    return parsed


def parse_symbol(token):
    return intern(token), None
        

def internal_read_quote(s):
    return read(s, one=True)


def read_quote(s):
    if s.next() == quote_char:
        els = internal_read_quote(s)
        # quote only supports one following exp
        if len(els) == 1:
            return True


def parse_quote(token):
    els = internal_read_quote(Stream(token, 1))
    r = [intern('quote'), *els]
    return r, None
    


readers_parsers = [
    (read_list, parse_list)
    , (read_whitespace, parse_whitespace)
    , (read_num, parse_num)
    , (read_quote, parse_quote)
    , (read_symbol, parse_symbol)
]

RETURN_ACTION = 'RETURN'


def read(s, readers=readers_parsers, one=False):
    r = []
    action = None
    while not s.empty() and action is not RETURN_ACTION:
        parsed = None
        for reader, parser in readers:
            istart = s.i
            valid = reader(s)
            if valid:
                token = s.program[istart:s.i]
                e, action = parser(token)
                if e is not None:
                    r.append(e)
                parsed = True
                break
            else:
                s.i = istart
        if not parsed:
            raise Exception('Unexpected: "%s" at %s' % (s.peek(), s.i))
        if one:
            break
    return r

