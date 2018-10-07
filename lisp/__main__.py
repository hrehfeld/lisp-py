from .symbol import intern
from .reader import read, Stream
from .interpreter import interpret

import unittest
    

def load_tests(loader, tests, pattern):
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
    ]

    class ReaderTestCase(unittest.TestCase):
        pass

    def make_test(program, expected_result):
        def testf(self):
            self.assertEqual(read(Stream(program, 0)), expected_result)
        return testf

    for itest, (program, expected_result) in enumerate(tests):
        setattr(ReaderTestCase, 'test_%s' % itest, make_test(program, expected_result))

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
        , ("(def foo) (set foo 1)", 1)
        , ("(def foo 1)", 1)
        , ("(def foo 1) foo", 1)
        , ("(def foo 1) (set foo 2) foo", 2)
        , ("(def foo (list 1 2)) foo", [1, 2])
    ]


    class InterpreterTestCase(unittest.TestCase):
        pass

    def make_test(program, expected_result):
        def testf(self):
            self.assertEqual(interpret(read(Stream(program, 0))), expected_result)
        return testf

    for itest, (program, expected_result) in enumerate(interpreter_tests):
        setattr(InterpreterTestCase, 'test_%s' % itest, make_test(program, expected_result))

    suite = unittest.TestSuite()
    suite.addTests(loader.loadTestsFromTestCase(ReaderTestCase))
    suite.addTests(loader.loadTestsFromTestCase(InterpreterTestCase))
    return suite

unittest.main()


