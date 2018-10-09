from .symbol import intern
from .reader import read, Stream
from .interpreter import interpret

import unittest
import argparse

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
    , ("'a", [[intern('quote'), intern('a')]])
    , ("'()", [[intern('quote'), []]])
    , ("()1", [[], 1])
    , ("'()1", [[intern('quote'), []], 1])
    , ("(defun foo () (+ 1 2)) (foo)", [[intern('defun'), intern('foo'), [], [intern('+'), 1, 2]], [intern('foo')]])
]

interpreter_tests = [
    ('''''', None)
    , ('1', 1)
    , ('1.0', 1.0)
    , ('t', True)
    , ('''(list)''', [])
    , ('''(list 1)''', [1])
    , ('''(quote ())''', [])
    , ("'1", 1)
    , ("'()", [])
    , ('''(+ 1 2)''', 3)
    , ("(def foo 2) (set foo 1)", 1)
    , ("(def foo 1)", 1)
    , ("(def foo 1) foo", 1)
    , ("(def foo 1) (set foo 2) foo", 2)
    , ("(def foo (list 1 2)) foo", [1, 2])
    , ("(defun foo ()) (foo)", None)
    , ("(defun foo () 1) (foo)", 1)
    , ("(defun foo () (+ 1 2)) (foo)", 3)
    , ("(defun foo (a) (+ a 2)) (foo 1)", 3)
    , ("""
(defun foo (b)
        (def a (+ 1 2))
        (* a b)
        )
        (foo 4)
""", 12)
]


def load_tests():
    def make_test(name, program, expected_result):
        def run(self):
            self.assertListEqual(expected_result, read(Stream(program, 0)))

        # have test name in stacktrace
        class testf(unittest.TestCase):
            pass
        setattr(testf, name, run)
        return testf(name)

    suite = unittest.TestSuite()
    for itest, (program, expected_result) in enumerate(tests):
        suite.addTest(make_test('reader_%s' % itest, program, expected_result))

    def make_test(name, program, expected_result):
        def run(self):
            self.assertEqual(interpret(read(Stream(program, 0))), expected_result)

        # have test name in stacktrace
        class testf(unittest.TestCase):
            pass
        setattr(testf, name, run)
        return testf(name)

    for itest, (program, expected_result) in enumerate(interpreter_tests):
        suite.addTest(make_test('interpreter_%s' % itest, program, expected_result))
        pass

    return suite

p = argparse.ArgumentParser()
p.add_argument('--type')
p.add_argument('--num', type=int)

args = p.parse_args()

if args.type is not None and args.num is not None:
    if args.type == 'reader':
        program = tests[args.num][0]
        print(read(Stream(program, 0)))
    if args.type == 'interpreter':
        program = interpreter_tests[args.num][0]
        print(interpret(read(Stream(program, 0))))
else:
    suite = load_tests()
    unittest.TextTestRunner().run(suite)


