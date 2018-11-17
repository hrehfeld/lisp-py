from .symbol import intern
from .reader import read, Stream
from .interpreter import interpret, Struct

import unittest
import argparse

reader_tests = [
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
    , ("a.b", [[intern('.'), intern('a'), intern('b')]])
    , (";a", [])
    , ("""(;a
)""", [[]])
    , ("'() ;1", [[intern('quote'), []]])
    , ("(defun foo () (+ 1 2)) (foo)", [[intern('defun'), intern('foo'), [], [intern('+'), 1, 2]], [intern('foo')]])
]

interpreter_tests = [
    ('''''', None)
    , ('1', 1)
    , ('1.0', 1.0)
    , ('nil', None)
    , ('true', True)
    , ('false', False)
    , ('''(list)''', [])
    , ('''(list 1)''', [1])
    , ('''(quote ())''', [])
    , ("'1", 1)
    , ("'()", [])
    , ("(nth 1 '(1 2 3)", 2)
    , ("(head '(1 2 3)", 1)
    , ("(tail '(1 2 3)", [2, 3])
    , ("(def v (tuple 1 2))", (1, 2))
    , ('''(+ 1 2)''', 3)
    , ('''(if 1 2 3)''', 2)
    , ('''(if 0 2 3)''', 3)
    , ('''(def b '()) (if b b 3)''', 3)
    , ("((fn (a) (+ a 2)) 1)", 3)
    , ("(call + 1 2)", 3)
    , ("(def foo 2) (call + 1 foo)", 3)
    , ("(call (fn (a) (+ a 2)) 1)", 3)
    , ("(apply (fn (a) (+ a 2)) '(1))", 3)
    , ("(call (fn (a &rest b) (+ a 2)) 1)", 3)
    , ("(call (fn (a &rest b) (+ a 2)) 1 2)", 3)
    , ("(call (fn (a &rest b) (if b (head b) a)) 1)", 1)
    , ("(call (fn (a &rest b) (if b (head b) a)) 1 2)", 2)
    , ("""
(call 
  (fn (a &rest b)
    (+ a (if b (head b) 2)))
  1 2)""", 3)
    , ("(call (fn (a) a) (+ 1 2))", 3)
    , ("(call (fn (a) a) (head '(3 2 1)))", 3)
    , ("(call (fn (a) a) (tail '(3 2 1)))", [2, 1])
    , ("(def foo 2) (set foo 1)", 1)
    , ("(def foo 1)", 1)
    , ("(def foo 1) foo", 1)
    , ("(def foo 1) (set foo 2) foo", 2)
    , ("(def foo 1) (set foo '()) foo", [])
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
    , ("(defun foo (a b c) (+ a (+ b c))) (foo 1 2 3)", 6)
    , ("(defun foo (&keys ks) ks) (foo :a 1)", dict(a=1))
    , ("(defun foo (&keys ks) ks) (foo :a 1 :b 2 :c 3)", dict(a=1, b=2, c=3))
    , ("(defun foo (a &keys ks) (tuple a ks)) (foo 1 :b 2)", (1, dict(b=2)))
    , ("(defun foo (a &keys ks) (tuple a ks)) (foo :a 1 :b 2 :c 3)", (1, dict(b=2, c=3)))
    , ("(defmacro add (a) (list '+ 5 a)) (add 1)", 6)
    , ("(defstruct Foo) (Foo)", Struct('Foo', [], []))
    , ("(defstruct Foo a) (Foo 1)", Struct('Foo', ['a'], [1]))
    , ("(defstruct Foo a) (def v (Foo 1)) (Foo-a v)", 1)
    , ("(defstruct Foo a) (def v (Foo 1)) v.a", 1)
    , ("(def symbols (dict :a 1))", dict(a=1))
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
    for itest, (program, expected_result) in enumerate(reader_tests):
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
p.add_argument('-c', nargs='+')
p.add_argument('--', dest='args', nargs='*')

args = p.parse_args()

if args.type is not None and args.num is not None:
    if args.type == 'reader':
        program = reader_tests[args.num][0]
        print(read(Stream(program, 0)))
    if args.type == 'interpreter':
        program = interpreter_tests[args.num][0]
        print(program)
        print(interpret(read(Stream(program, 0))))
elif args.c:
    for filename in args.c:
        with open(filename, 'r') as f:
            program = f.read()
        print(interpret(read(Stream(program, 0)), args=args.args))
else:
    suite = load_tests()
    unittest.TextTestRunner().run(suite)


