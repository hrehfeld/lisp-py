from .base import TYPE, TYPE_T
from .symbol import intern
from .reader import read, Stream, quote_fun_name, backquote_fun_name, backquote_eval_fun_name, backquote_splice_fun_name
from .interpreter import interpret, ps

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
    , ("'a", [[intern(quote_fun_name), intern('a')]])
    , ("'()", [[intern(quote_fun_name), []]])
    , ("()1", [[], 1])
    , ("'()1", [[intern(quote_fun_name), []], 1])
    , ("`()", [[intern(backquote_fun_name), []]])
    , ("`~foo", [[intern(backquote_fun_name), [intern(backquote_eval_fun_name), intern('foo')]]])
    , ("`(~foo)", [[intern(backquote_fun_name), [[intern(backquote_eval_fun_name), intern('foo')]]]])
    , ("`(bar ~foo)", [[intern(backquote_fun_name), [intern('bar'), [intern(backquote_eval_fun_name), intern('foo')]]]])
    , ("`(bar ~foo baz)", [[intern(backquote_fun_name), [intern('bar'), [intern(backquote_eval_fun_name), intern('foo')], intern('baz')]]])
    , ("`(bar ~@ foo baz)", [[intern(backquote_fun_name), [intern('bar'), [intern(backquote_splice_fun_name), intern('foo')], intern('baz')]]])
    , ("a.b", [[intern('.'), intern('a'), intern('b')]])
    , (";a", [])
    , ("""(;a
)""", [[]])
    , ("'() ;1", [[intern(quote_fun_name), []]])
    , ("(defun foo () (+ 1 2)) (foo)", [[intern('defun'), intern('foo'), [], [intern('+'), 1, 2]], [intern('foo')]])
]

interpreter_tests = [
    ('''''', None)
    # TODO check if ending ) is checked
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
    , ("`1", 1)
    , ("`\"test\"", 'test')
    , ("`foo", intern('foo'))
    , ("`1", 1)
    , ("`()", [])
    , ("`(foo)", [intern('foo')])
    , ("(let* ((foo 3)) `(~foo))", [3])
    , ("(let* ((foo 3)) `(bar ~foo))", [intern('bar'), 3])
    , ("(let* ((foo 3)) `(bar ~@(list foo 1)))", [intern('bar'), 3, 1])
    , ("(destructuring-bind-parse 'target 0", [[intern('target'), 0]])
    , ("(destructuring-bind-parse '(targeta targetb) 'value)", [[intern('targeta'), [intern('nth'), 0, intern('value')]]
                                                                , [intern('targetb'), [intern('nth'), 1, intern('value')]]])
    , ("(destructuring-bind-parse '(targeta (targetb targetc)) 'value)"
       , [[intern('targeta'), [intern('nth'), 0, intern('value')]]
          , [intern('targetb'), [intern('nth'), 0, [intern('nth'), 1, intern('value')]]]
          , [intern('targetc'), [intern('nth'), 1, [intern('nth'), 1, intern('value')]]]])
    , ("(destructuring-bind-parse '((targeta targetb) targetc) 'value)"
       , [[intern('targeta'), [intern('nth'), 0, [intern('nth'), 0, intern('value')]]]
          , [intern('targetb'), [intern('nth'), 1, [intern('nth'), 0, intern('value')]]]
          , [intern('targetc'), [intern('nth'), 1, intern('value')]]])
    , ("(setf target 0) target", 0)
    , ("(setf (tuple targeta targetb) '(1 2)) (list targeta targetb)"
       , [1, 2])
    , ("(let* ((l (list 1))) (setf (aref l 0) 0) l)", [0])
    , ("(let* ((l (dict :foo 1))) (setf (aref l :foo) 0) l)", dict(foo=0))
    , ("(let ((foo 3)) foo)", 3)
    , ("(let (((foo bar) (list 0 1))) (list foo bar))", [0, 1])
    # expected error
    #, ("(let (((foo bar baz) (list 0 1))) (list foo bar))", [0, 1])
    # expected error
    #, ("(let (((foo) (list 0 1))) foo)", 0)
    # expected error
    #, ("(let ((foo 3)) `(bar ,(+ ,foo 1)))", [intern('bar'), [intern('+'), 3, 1]])
    , ("(let ((foo 3)) `(bar (+ ~foo 1)))", [intern('bar'), [intern('+'), 3, 1]])
    , ("(nth 1 '(1 2 3)", 2)
    , ("(head '(1 2 3)", 1)
    , ("(tail '(1 2 3)", [2, 3])
    , ("(reversed '(1 2 3)", [3, 2, 1])
    , ("(let ((l '())) (append l 3) l)", [3])
    , ("(tuple 1 2)", (1, 2))
    , ("(tuple 'x 2)", (intern("x"), 2))
    , ("(def v (tuple 1 2))", (1, 2))
    , ("(def v (tuple 1 2))", (1, 2))
    , ('''(+ 1 2)''', 3)
    , ("(let* ((foo 3)) (+= foo 1))", 4)
    , ("(let* ((foo 3)) (+= foo 1 2))", 6)
    , ("(let* ((foo 3) (bar 2)) (+= foo 1 bar))", 6)
    , ("(let ((foo 3)) (+= foo 1))", 4)
    , ("(let ((foo 3)) (+= foo 1 2))", 6)
    , ("(let ((foo 3) (bar 2)) (+= foo 1 bar))", 6)
    , ("(let ((foo 3)) (-= foo 1))", 2)
    , ("(let ((foo 3)) (-= foo 1 2))", 0)
    , ("(let ((foo 3) (bar 2)) (-= foo 1 bar))", 0)
    , ("(let ((foo 3)) (*= foo 2))", 6)
    , ("(let ((foo 3)) (*= foo 1 2))", 6)
    , ("(let ((foo 3) (bar 2)) (*= foo 1 bar))", 6)
    , ("(let ((foo 6)) (/= foo 2))", 3)
    , ("(let ((foo 6)) (/= foo 1 2))", 3)
    , ("(let ((foo 6) (bar 2)) (/= foo 1 bar))", 3)
    , ('''(if 1 2 3)''', 2)
    , ('''(if 0 2 3)''', 3)
    , ('''(if 0 2)''', None)
    , ('''(when false 1)''', None)
    , ('''(when 1 1)''', 1)
    , ('''(def b '()) (if b b 3)''', 3)
    , ("((fn (a) (+ a 2)) 1)", 3)
    , ("(call + 1 2)", 3)
    , ("(def foo 2) (call + 1 foo)", 3)
    , ("(call (fn (a) (+ a 2)) 1)", 3)
    , ("(apply (fn (a) (+ a 2)) '(1))", 3)
    , ("(apply (fn (a) a) (list 'foo))", intern("foo"))
    , ("(apply (fn (a) a) (list (quote foo))", intern("foo"))
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
    , ("(eval 1)", 1)
    , ("(eval '())", [])
    , ("(eval (+ 1 2))", 3)
    , ("(setf foo (+ 1 2)) foo", 3)
    , ("""
(def foo0)
(def foo1)
(def foo2)
(setf (tuple foo0 foo1 foo2) '(0 1 2))
(tuple foo2 foo1 foo0)
""", (2, 1, 0))
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
    , ("(defun foo (a) a) (foo :a 1)", 1)
    , ("(defun foo (a) a) (foo :a :b)", intern(':b'))
    , ("(defun foo (a) a) (foo :a :a)", intern(':a'))
    , ("(defun foo (&nokeys a b) (list a b)) (foo :a 0)", [intern(':a'), 0])
    , ("(defun foo (&keys ks) ks) (foo :a 1)", dict(a=1))
    , ("(defun foo (&keys ks) ks) (foo :a 1 :b 2 :c 3)", dict(a=1, b=2, c=3))
    , ("(defun foo (a &keys ks) (tuple a ks)) (foo 1 :b 2)", (1, dict(b=2)))
    , ("(defun foo (a &keys ks) (tuple a ks)) (foo :a 1 :b 2 :c 3)", (1, dict(b=2, c=3)))
    , ("(defmacro add (a) (list '+ 5 a)) (add 1)", 6)

    , ("(defstruct Foo) (Foo)", {TYPE: {TYPE: TYPE_T, 'name': 'Foo', 'fields': tuple()}})
    , ("(defstruct Foo a) (Foo 1)", {TYPE: {TYPE: TYPE_T, 'name': 'Foo', 'fields':tuple('a')}, 'a': 1})
    , ("(defstruct Foo a) (def v (Foo 1)) (Foo-a v)", 1)
#    , ("(defstruct Foo a) (def v (Foo 1)) v.a", 1)
    , ("(def foo (dict :a 1))", dict(a=1))
    , ("""(let ((n 0)) (dolist (i '(0 1 2 3 4)) (set n (+ n i))) n)""", 10)
    , ("""(let ((n 3)) (dolist (i '()) (set n (+ n i))) n)""", 3)
    , ("""
(let ((n nil))
  (dolist (i '(foo bar baz))
    (print (repr i) (symbol? i))
    (when (eq i 'foo)
      (set n i)
      (print (repr n))))
    n)""", intern('foo'))
    , ("""(let ((r '())) (dolist (i '(0 1 2 3 4)) (append r i)) r)""", [0, 1, 2, 3, 4])
    , ("""
(let ((x 5))
  (cond ((< x 3) 7)
    (true 1)))
""", 1)
    , ("""
(let ((x 5))
  (cond 
    ((< x 3) 5)
    ((< x 9) 7)
    (true 1)))
""", 7)
    , ("""(block nil  (return-from nil nil))""", None)
    , ("""(block nil  (return-from nil 1))""", 1)
    , ("""(block test (return-from test nil))""", None)
    , ("""(block test (return-from test 1))""", 1)
   , ("""(block nil (break nil))""", None)
   , ("""(block nil (break 1))""", 1)
    , ("""(map (fn (e) (+ e 1)) '(0 1 2))""", [1, 2, 3])
    , ("""(list :a)""", [intern(':a')])
    , ("""(list :a 0)""", [intern(':a'), 0])
    , ("""(range 4)""", [0, 1, 2, 3])
    , ("""(range 0 4)""", [0, 1, 2, 3])
    , ("""(range 1 4)""", [1, 2, 3])
    , ("""(range 1 4 2)""", [1, 3])
    , ("""(range 1 5 3)""", [1, 4])
    , ("""(enumerate '(0 1 2))""", [[0, 0], [1, 1], [2, 2]])
    , ("""(enumerate '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip '(0 1 2) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip '(0 1 2 3 4) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip '(0 1 2 3) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip (range 3) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip (range 5) '(foo bar baz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
    , ("""(zip (range 3) '(foo bar baz biz))""", [[0, intern('foo')], [1, intern('bar')], [2, intern('baz')]])
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
        ps(read(Stream(program, 0)))
    if args.type == 'interpreter':
        program = interpreter_tests[args.num][0]
        print(program)
        ps(interpret(read(Stream(program, 0))))
elif args.c:
    for filename in args.c:
        with open(filename, 'r') as f:
            program = f.read()
        print(interpret(read(Stream(program, 0)), args=args.args))
else:
    suite = load_tests()
    unittest.TextTestRunner().run(suite)


