from .symbol import intern
from .reader import read, Stream

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
    , ('''(())''', [[[]]])
    , ('''((list a b ()))''', [[[intern('list'), intern('a'), intern('b'), []]]])
]


for program, expected_result in programs:
    print('STARTING')
    print('   ', program)
    sexps = read(Stream(program, 0))
    for sexp in sexps:
        print(str(sexp))
    for sexp in expected_result:
        print(str(sexp))
    assert(sexps == expected_result)

