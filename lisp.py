from collections import OrderedDict as odict

whitespace = ' \t\n'
token_end_chars = whitespace + ')'

floating_point = '.'

symbols = {}

class Symbol:
    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s

    def __str__(self):
        return self.s


def intern(s):
    symbols.setdefault(s, Symbol(s))
    return symbols[s]


def ends_token(program, i):
    """assumes that all tokens are ended by one of token_end_chars"""
    return program[i] in token_end_chars


def is_paren_open(c):
    return c == '('


def is_paren_close(c):
    return c == ')'


def is_whitespace(c):
    return c in " \n\t"


def next_token_is(reader, program, i):
    return reader(program, i) != 0


def read_list(program, i):
    c = program[i]
    print(c, i)
    if not is_paren_open(c):
        return 0
    _, i = read_sublist(program, i + 1)
    print('is paren')
    return 1 + i


def read_sublist(program, i):
    def list_end(program, ilast, i):
        return None, i, STOP_ACTION

    els, i = read(program, i, readers=[(read_list_end, list_end)] + readers)
    assert(read_list_end(program, i))
    return els, i


def parse_list(program, ilast, i):
    els, i = read_sublist(program, ilast + 1)
    return els, i + 1, None


def read_list_end(program, i):
    return 1 if is_paren_close(program[i]) else 0


def _read_int(program, istart):
    i = istart
    while i < len(program) and program[i] in '0123456789':
        i += 1
    return i - istart


def read_num(program, istart):
    print('read num', program[istart:], istart)
    i = istart + _read_int(program, istart)
    print(program[istart:i], istart, i, len(program))
    if i < len(program) and program[i] in floating_point:
        i = i + 1 + _read_int(program, i + 1)
    print(program[istart:i])
    if i < len(program) and not ends_token(program, i):
        return 0
    return i - istart


def parse_num(program, ilast, i):
    num = float if floating_point in program[ilast:i] else int
    print('creating %s %s' % (num, program[ilast:i]))
    return num(program[ilast:i]), i, None
    


def read_whitespace(program, istart):
    i = istart
    print('read_whitespace', program[i:])
    while i < len(program) and program[i] in whitespace:
        i += 1
    return i - istart


def parse_whitespace(program, ilast, i):
    return None, i, None
        

def read_symbol(program, istart):
    i = istart
    while i < len(program):
        for reader in [read_list_end] + [r for r, p in readers if r is not read_symbol]:
            if next_token_is(reader, program, i):
                return i - istart
        i += 1
    return i - istart


def parse_symbol(program, ilast, i):
    return intern(program[ilast:i]), i, None
        

readers = [
    (read_list, parse_list)
    , (read_whitespace, parse_whitespace)
    , (read_num, parse_num)
    , (read_symbol, parse_symbol)
]

STOP_ACTION = 'STOP'


def read(program, start_i=0, readers=readers):
    in_list = start_i != 0
    i = start_i
    print('read %s %s' % (i, in_list))
    r = []
    while i < len(program):
        c = program[i]

        print('CHAR', c)
        parsed = None
        action = None
        for reader, parser in readers:
            print(reader)
            num_read = reader(program, i)
            if num_read > 0:
                print(reader, num_read)
                e, i, action = parser(program, i, i + num_read)
                print('after parse', e, i, action)
                if e is not None:
                    r.append(e)
                parsed = True
                break
            else:
                print('ignore')
        if not parsed:
            raise Exception('Unexpected: "%s"' % program[i:])
        if action is STOP_ACTION:
            break
    return r, i - 1

if __name__ == '__main__':
    programs = [
        ('''''', [])
        , ('1', [1])
        , ('1.0', [1.0])
        , ('''()''', [[]])
        , ('''(1)''', [[1]])
        , ('''(1 2)''', [[1, 2]])
        , ('''(foo)''', [[intern('foo')]])
        , ('''(foo bar)''', [[intern('foo'), intern('bar')]])
        , ('''(1+)''', [[intern('1+')]])
    ]


    for program, expected_result in programs:
        print('STARTING')
        print('   ', program)
        sexps, _ = read(program)
        for sexp in sexps:
            print(str(sexp))
        for sexp in expected_result:
            print(str(sexp))
        assert(sexps == expected_result)

