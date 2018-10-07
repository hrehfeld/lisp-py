from .symbol import intern

whitespace = ' \t\n'
token_end_chars = whitespace + ')'

floating_point = '.'


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


def read_list(s):
    if s.empty() or not is_paren_open(s.peek()):
        return None
    s.next()
    _ = read_sublist(s)
    return True


def read_sublist(s):
    def parse_list_end(token):
        return None, STOP_ACTION

    els = read(s, readers=[(read_list_end, parse_list_end)] + readers)
    return els


def parse_list(token):
    els = read_sublist(Stream(token, 1))
    return els, None


def read_list_end(s):
    if not s.empty() and is_paren_close(s.peek()):
        s.next()
        return True


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
        

readers = [
    (read_list, parse_list)
    , (read_whitespace, parse_whitespace)
    , (read_num, parse_num)
    , (read_symbol, parse_symbol)
]

STOP_ACTION = 'STOP'


def read(s, readers=readers):
    r = []
    while not s.empty():
        parsed = None
        action = None
        for reader, parser in readers:
            i = s.i
            valid = reader(s)
            if valid:
                e, action = parser(s.program[i:s.i])
                if e is not None:
                    r.append(e)
                parsed = True
                break
            else:
                s.i = i
        if not parsed:
            raise Exception('Unexpected: "%s"' % s.peek())
        if action is STOP_ACTION:
            break
    return r

