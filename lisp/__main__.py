from .symbol import intern
from .reader import read, Stream
from .interpreter import interpret

tests = [
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

interpreter_tests = [
    ('''''', None)
    , ('1', 1)
    , ('1.0', 1.0)
    , ('t', True)
    , ('''(list)''', [])
    , ('''(list 1)''', [1])
    , ('''(quote ())''', [])
]


for program, expected_result in tests:
    print('STARTING')
    print('   ', program)
    sexps = read(Stream(program, 0))
    for sexp in sexps:
        print(str(sexp))
    for sexp in expected_result:
        print(str(sexp))
    assert(sexps == expected_result)

print('----interpret')
for program, expected_result in interpreter_tests:
    print('Program:', program)
    r = interpret(read(Stream(program, 0)))
    print('%s == %s' % (r, expected_result))
    assert(r == expected_result)
