;; warning: test skipped: ""
;; TODO check if ending ) is checked


((progn 1) 1 )
((progn 1.0) 1.0 )
((progn nil) nil )
((progn true) True )
((progn false) False )
((progn (list)) () )
((progn (list 1)) ( 1) )
((progn (quote ())) () )
((progn '1) 1 )
((progn '()) () )
((progn `1) 1 )
((progn `"test") "test" )
((progn `foo) foo )
((progn `1) 1 )
((progn `()) () )
((progn `(foo)) ( foo) )
;;  expected error
;; , ("(let (((tuple foo bar baz) (list 0 1))) (list foo bar))", [0, 1])
;;  expected error
;; , ("(let (((tuple foo) (list 0 1))) foo)", 0)
;;  expected error
;; , ("(let ((foo 3)) `(bar ,(+ ,foo 1)))", [intern('bar'), [intern('+'), 3, 1]])
((progn (let* ((foo 3)) `~foo)) 3 )
((progn (let* ((foo 3)) `(~foo))) ( 3) )
((progn (let* ((foo 3)) `(bar ~foo))) ( bar 3) )
((progn (let* ((foo 3)) `(bar ~@(list foo 1)))) ( bar 3 1) )
((progn (let* ((foo 3)) `(bar `(~foo)))) ( bar (backquote 3)) )
((progn (let* ((foo 3)) `(bar ~@(list foo 1)))) ( bar 3 1) )
((progn (destructuring-bind-parse 'target 0)) ( (target 0)) )
((progn (destructuring-bind-parse '(targeta targetb) 'value)) ( (targeta (nth 0 value)) (targetb (nth 1 value))) )
((progn (destructuring-bind-parse '(targeta (targetb targetc)) 'value)) ( (targeta (nth 0 value)) (targetb (nth 0 (nth 1 value))) (targetc (nth 1 (nth 1 value))))
)
((progn (destructuring-bind-parse '((targeta targetb) targetc) 'value)) ( (targeta (nth 0 (nth 0 value))) (targetb (nth 1 (nth 0 value))) (targetc (nth 1 value)))
)
((progn (def target) (setf target 0) target) 0 )
((progn (def targeta)
        (def targetb)
        (setf (:= targeta targetb) '(1 2))
        (list targeta targetb))
 ( 1 2) )
((progn (let* ((l (list 1))) (setf (aref l 0) 0) l)) ( 0) )
((progn (let* ((l (dict :foo 1))) (setf (aref l :foo) 0) l)) (dict :foo 0 ) )
((progn (let ((foo 3)) foo)) 3 )
((progn (let (((:= foo bar) (list 0 1))) (list foo bar))) ( 0 1) )
((progn (let ((foo 3)) `(bar (+ ~foo 1)))) ( bar (+ 3 1)) )
((progn (nth 1 '(1 2 3))) 2 )
((progn (head '(1 2 3))) 1 )
((progn (tail '(1 2 3))) ( 2 3) )
((progn (reversed '(1 2 3))) ( 3 2 1) )
((progn (let ((l '())) (append l 3) l)) ( 3) )
((progn (tuple 1 2)) ( 1 2) )
((progn (tuple 'x 2)) ( x 2) )
((progn (def v (tuple 1 2))) ( 1 2) )
((progn (def v (tuple 1 2))) ( 1 2) )
((progn (+ 1 2)) 3 )
((progn (let* ((foo 3)) (+= foo 1))) 4 )
((progn (let* ((foo 3)) (+= foo 1 2))) 6 )
((progn (let* ((foo 3) (bar 2)) (+= foo 1 bar))) 6 )
((progn (let* ((foo 3)) (+= foo 1))) 4 )
((progn (let* ((foo 3)) (+= foo 1 2))) 6 )
((progn (let* ((foo 3) (bar 2)) (+= foo 1 bar))) 6 )
((progn (let* ((foo 3)) (-= foo 1))) 2 )
((progn (let* ((foo 3)) (-= foo 1 2))) 0 )
((progn (let* ((foo 3) (bar 2)) (-= foo 1 bar))) 0 )
((progn (let* ((foo 3)) (*= foo 2))) 6 )
((progn (let* ((foo 3)) (*= foo 1 2))) 6 )
((progn (let* ((foo 3) (bar 2)) (*= foo 1 bar))) 6 )
((progn (let* ((foo 6)) (/= foo 2))) 3 )
((progn (let* ((foo 6)) (/= foo 1 2))) 3 )
((progn (let* ((foo 6) (bar 2)) (/= foo 1 bar))) 3 )
((progn (if 1 2 3)) 2 )
((progn (if 0 2 3)) 3 )
((progn (if 0 2)) nil )
((progn (when false 1)) nil )
((progn (when 1 1)) 1 )
((progn (def b '()) (if b b 3)) 3 )
((progn ((fn (a) (+ a 2)) 1)) 3 )
((progn (apply (fn (a) (+ a 2)) '(1))) 3 )
((progn (apply (fn (a) a) (list 'foo))) foo )
((progn (apply (fn (a) a) (list (quote foo)))) foo )
((progn ((fn (a) (+ a 2)) 1)) 3 )
((progn ((fn (a &rest b) (+ a 2)) 1)) 3 )
((progn ((fn (a &rest b) (+ a 2)) 1 2)) 3 )
((progn ((fn (a &rest b) (if b (head b) a)) 1)) 1 )
((progn ((fn (a &rest b) (if b (head b) a)) 1 2)) 2 )
((progn 
(
  (fn (a &rest b)
    (+ a (if b (head b) 2)))
  1 2)) 3 )
((progn ((fn (a) a) (+ 1 2))) 3 )
((progn ((fn (a) a) (head '(3 2 1)))) 3 )
((progn ((fn (a) a) (tail '(3 2 1)))) ( 2 1) )
((progn (def foo 2) (set foo 1)) 1 )
((progn (def foo 1)) 1 )
((progn (def foo 1) foo) 1 )
((progn (def foo 1) (set foo 2) foo) 2 )
((progn (def foo 1) (set foo '()) foo) () )
((progn (def foo (list 1 2)) foo) ( 1 2) )
((progn (eval 1)) 1 )
((progn (eval '())) () )
((progn (eval (+ 1 2))) 3 )
((progn (def foo) (setf foo (+ 1 2)) foo) 3 )
((progn 
(def foo0)
(def foo1)
(def foo2)
(setf (:= foo0 foo1 foo2) '(0 1 2))
(tuple foo2 foo1 foo0)
) ( 2 1 0) )
((progn (defun foo ()) (foo)) nil )
((progn (defun foo () 1) (foo)) 1 )
((progn (defun foo () (+ 1 2)) (foo)) 3 )
((progn (defun foo (a) (+ a 2)) (foo 1)) 3 )
((progn 
(defun foo (b)
        (def a (+ 1 2))
        (* a b)
        )
        (foo 4)
) 12 )
((progn (defun foo (a b c) (+ a (+ b c))) (foo 1 2 3)) 6 )
((progn (defun foo (a) a) (foo :a 1)) 1 )
((progn (defun foo (a) a) (foo :a :b)) :b )
((progn (defun foo (a) a) (foo :a :a)) :a )
((progn (defun foo (&nokeys a b) (list a b)) (foo :a 0)) ( :a 0) )
((progn (defun foo (&keys ks) ks) (foo :a 1)) (dict :a 1 ) )
((progn (defun foo (&keys ks) ks) (foo :a 1 :b 2 :c 3)) (dict :a 1  :b 2  :c 3 ) )
((progn (defun foo (a &keys ks) (tuple a ks)) (foo 1 :b 2)) ( 1 (dict :b 2)) )
((progn (defun foo (a &keys ks) (tuple a ks)) (foo :a 1 :b 2 :c 3)) ( 1 (dict :b 2 :c 3)) )
((progn (defmacro add (a) (list '+ 5 a)) (add 1)) 6 )
((progn (defstruct Foo) (Foo)) (Foo) )
((progn (defstruct Foo a) (Foo 1)) (Foo :a 1) )
((progn (defstruct Foo a) (def v (Foo 1)) (Foo-a v)) 1 )
((progn (def foo (dict :a 1))) (dict :a 1 ) )
((progn (let ((n 0)) (dolist (i '(0 1 2 3 4)) (set n (+ n i))) n)) 10 )
((progn (let ((n 3)) (dolist (i '()) (set n (+ n i))) n)) 3 )
((progn 
(let ((n nil))
  (dolist (i '(foo bar baz))
    (when (eq i 'foo)
      (set n i)))
    n)) foo )
((progn (let ((r '())) (dolist (i '(0 1 2 3 4)) (append r i)) r)) ( 0 1 2 3 4) )
((progn (foldr + 0 '(0 1 2 3 4))) 10 )
((progn 
(let ((x 5))
  (cond ((< x 3) 7)
    (true 1)))
) 1 )
((progn 
(let ((x 5))
  (cond 
    ((< x 3) 5)
    ((< x 9) 7)
    (true 1)))
) 7 )
((progn 
(let ((x 5))
  (cond 
    ((< x 3) 5)
    ((< x 9) 7)))
) 7 )
((progn (block foo (return-from foo))) nil )
((progn (block foo (return-from foo nil))) nil )
((progn (block foo (return-from foo 1))) 1 )
((progn (block test (return-from test nil))) nil )
((progn (block test (return-from test 1))) 1 )
((progn (block nil (break nil))) nil )
((progn (block nil (break 1))) 1 )
((progn (map (fn (e) (+ e 1)) '(0 1 2))) ( 1 2 3) )
((progn (list :a)) ( :a) )
((progn (list :a 0)) ( :a 0) )
((progn (defun foo (a) a) (foo :a)) :a )
((progn (defun foo (a b) a) (foo &nokeys :a 0)) :a )
((progn (range 4)) ( 0 1 2 3) )
((progn (range 0 4)) ( 0 1 2 3) )
((progn (range 1 4)) ( 1 2 3) )
((progn (range 1 4 2)) ( 1 3) )
((progn (range 1 5 3)) ( 1 4) )
((progn (enumerate '(0 1 2))) ( (0 0) (1 1) (2 2)) )
((progn (enumerate '(foo bar baz))) ( (0 foo) (1 bar) (2 baz)) )
((progn (zip '(0 1 2) '(foo bar baz))) ( (0 foo) (1 bar) (2 baz)) )
((progn (zip '(0 1 2 3 4) '(foo bar baz))) ( (0 foo) (1 bar) (2 baz)) )
((progn (zip '(0 1 2 3) '(foo bar baz))) ( (0 foo) (1 bar) (2 baz)) )
((progn (zip (range 3) '(foo bar baz))) ( (0 foo) (1 bar) (2 baz)) )
((progn (zip (range 5) '(foo bar baz))) ( (0 foo) (1 bar) (2 baz)) )
((progn (zip (range 3) '(foo bar baz biz))) ( (0 foo) (1 bar) (2 baz)) )
((progn (dolist (i (range 99)))) nil )
((progn (slice (range 9) 3)) ( 0 1 2) )
((progn (slice (range 9) 3 7)) ( 3 4 5 6) )
((progn (slice (range 9) 3 -2)) ( 3 4 5 6) )
((progn (slice (range 9) 3 7 2)) ( 3 5) )
((progn (slice (range 3) -1)) ( 0 1) )
((progn (slice (range 9) -8)) ( 0) )
((progn (slice (range 9) -7)) ( 0 1) )
((progn (slice (range 9) -3 -1)) ( 6 7) )
((progn (slice (range 9) -3 0)) ( 6 7 8) )
((progn
   (map-apply
    (fn (a) a)
    '((1))))
 nil)
((progn
   (map-apply
    (fn (a b) (+ a b))
    '((1 2))))
 nil)
((progn
   (map-apply
    ( fn (var val) (assert (symbol? var) (repr var)) (list 'set var val))
    ( destructuring-bind-parse '(a b) 'value) ))
 nil)
((progn
   (let* ((value-var 'evaled-value-var)
          (value '(evaled-value-a evaled-value-b))
          (target '(:= foo bar)))
     `(progn
       ~@(map-apply
          (fn (var val)
              (assert (symbol? var) (repr var))
              (list 'set var val))
          (destructuring-bind-parse (as-list (slice target 1 nil)) value-var)))))
 (+
 (list 'progn (list 'def value-var value))
 (cond
  ((symbol? target)
   `((set ~target ~value-var)))
  ((named-operator? target ':=)
   (map-apply
    (fn (var val)
        (assert (symbol? var) (repr var))
        (list 'set var val))
    (destructuring-bind-parse (as-list (slice target 1 nil)) value-var)))
  ((named-operator? target 'aref)
   (let* ((target (tail target)))
     (assert (eq (length target) 2) (repr target))
     (let* ((obj (1st target)) (key (2nd target)))
       (when (keyword? key) (set key (keyword-name key)))
       `((if (list? ~obj)
             (progn (assert (num? ~key) (repr ~key)) (list-set ~obj ~key ~value-var))
           (assert (dict? ~obj) (repr ~obj))
           (dict-set ~obj ~key ~value-var))))))
 (true (throw (Exception (+ "unknown target" (repr target))))))
 ))
((progn
   (backquote-internal '(defmacro ~name (var &rest args)
                          `foo)
                       false))
   '(list 'defmacro name (list 'var '&rest 'args)
          (list 'backquote 'foo)))
((progn
   (backquote-internal '(defmacro ~name (var &rest args)
                          `(setf ~~var (apply ~op (list ~~var ~~@args))))
                       false))
   '(list 'defmacro name (list 'var '&rest 'args)
          (list 'backquote (list 'setf '~var (list 'apply op (list 'list '~var '~@args))))))
((progn
   (let* ((value-var 'evaled-value-var)
          (value 'evaled-value)
          (target 'foo))
     (backquote  (progn
                      (def ~value-var ~value)
                      ~@(cond
                         ((symbol? target)
                          `((set ~target ~value-var)))
                         ((named-operator? target ':=)
                          (map-apply (fn (var val)
                                         (assert (symbol? var) (repr var))
                                         (list 'set var val))
                                     (destructuring-bind-parse (as-list (slice target 1 nil)) value-var)))
                         ((named-operator? target 'aref)
                          (let*
                              ((target (tail target)))
                            (assert (eq (length target) 2) (repr target))
                            (let*
                                ((obj (1st target)) (key (2nd target)))
                              (when (keyword? key) (set key (keyword-name key)))
                              `((if (list? ~obj)
                                    (progn (assert (num? ~key) (repr ~key)) (list-set ~obj ~key ~value-var))
                                  (assert (dict? ~obj) (repr ~obj))
                                  (dict-set ~obj ~key ~value-var))))))
                         (true (throw (Exception (+ "unknown target" (repr target))))))))))
 (+
 (list 'progn (list 'def value-var value))
 (cond
  ((symbol? target)
   `((set ~target ~value-var)))
  ((named-operator? target ':=)
   (map-apply
    (fn (var val)
        (assert (symbol? var) (repr var))
        (list 'set var val))
    (destructuring-bind-parse (as-list (slice target 1 nil)) value-var)))
  ((named-operator? target 'aref)
   (let* ((target (tail target)))
     (assert (eq (length target) 2) (repr target))
     (let* ((obj (1st target)) (key (2nd target)))
       (when (keyword? key) (set key (keyword-name key)))
       `((if (list? ~obj)
             (progn (assert (num? ~key) (repr ~key)) (list-set ~obj ~key ~value-var))
           (assert (dict? ~obj) (repr ~obj))
           (dict-set ~obj ~key ~value-var))))))
 (true (throw (Exception (+ "unknown target" (repr target))))))
 ))
((progn (macroexpand-1 (setf (:= targeta targetb) '(0 1))))
 nil)
