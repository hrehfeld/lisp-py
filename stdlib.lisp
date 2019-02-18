(defun atom? (e)
  (or (num? e) (str? e) (keyword? e) (symbol? e) (nil? e)))

(defun cons:alloc (n)
  (let* ((i 0)
         (r nil))
    (__while (< i n)
             (set r (cons nil r))
             (set i (1+ i)))
    r))

;; TODO uses eval -- can we do without or do we need to replace with compiler internal backquote?
(defun backquote-internal (form nested)
  ;; avoid circular macros -- almost everything else uses backquote itself
  ;; unquote
  (let* ((single (fn (e) (if nested e (list 'list e))))
         (quoted (fn (e) (if nested e (list 'quote e)))))
    (if (named-operator? form 'unquote)
        (progn
          (assert (eq (length form) 2)
                  (repr form))
          (let* ((unquoted-form (cadr form)))
            ;; nested unquote suppresses eval
            (if (or (named-operator? unquoted-form 'unquote)
                    (named-operator? unquoted-form 'unquote-splice))
                (single (quoted unquoted-form))
              (single unquoted-form))))
      ;; unquote splice
      (if (named-operator? form 'unquote-splice)
          (progn 
            (assert (eq (length form) 2)) 
            (let* ((form (cadr form)) 
                   ;;(r (eval form))
                   ) 
              ;;(assert (list? r) (tuple r (sexps_str form)))
              form))
        ;; backquote
        (if (named-operator? form 'backquote)
            (progn 
              (assert (eq (length form) 2)) 
              (let* ((backquoted-form (cadr form)) 
                     (r (backquote-internal backquoted-form false)))
                ;;(print "inner backquote" (repr r))
                (assert (list? r) r)
                (assert (eq  (length r) 2) r)
                (assert (__is (car r) 'list) r)
                (single (list 'list '(quote backquote) (cadr r)))))
          ;; generic list case
          (if (list? form)
              (let* ((r nil))
                ;;(print "LISTTTTTTTTTTTT" (repr form))
                (__while (length form)
                         ;;(print "element" (repr (car form)))
                         (let* ((e (car form))
                                (e (backquote-internal e nested)))
                           (set form (cdr form))
                           ;; if both last r and e are 'list (constructors), just append e to last r
                           (if (and (length r)
                                    (list? (car r))
                                    (length (car r))
                                    (__is (car (car r)) 'list)
                                    (list? e)
                                    (length e)
                                    (__is (car e) 'list)
                                    )
                               (setcar r (cons:append (car r)
                                                      ;; cut off 'list
                                                      (cdr e)))
                             (set r  (cons e r)))
                           ;;(print "backquote-internal" (repr e) "\nr:" (repr r))
                           ))
                (set r (reverse r))
                ;; add list concatenation if we need it or remove list wrap if only one element
                (if (not nested)
                    (if  (> (length r) 1)
                        (set r (cons 'cons:append r))
                      (assert (<= (length r) 2))
                      (if (> (length r) 0)
                          (set r (last r)))))
                (single r))
            ;; atom
            (assert (atom? form) (repr form))
            (single (quoted form))))))))

(defmacro backquote (form)
  (let* ((r (backquote-internal form false)))
    (assert (eq (length r) 2) (repr r))
    (assert (__is (car r) 'list) (repr r))
    (cadr r)))

(defmacro when (test &rest body)
  `(if ~test (progn ~@body)))

(defmacro unless (test &rest body)
  `(if (not ~test) (progn ~@body)))

(defmacro block (name &rest body)
  (assert (symbol? name) name)
  (let* ((name (if (__is name 'nil)
				   (gensym block)
				 name)))
	`(internal:block ~name
			  (defun break ((value nil)) (return-from ~name value))
			  ~@body)))

(defmacro while (test &rest body)
  `(block ~(gensym while-block)
	      (__while ~test ~@body)))

(defmacro dolist (iter &rest body)
  (assert (eq (length iter) 2))
  (let* ((var-sym (head iter))
         (let-sym (if (symbol? var-sym) 'let* 'let))
         (list-value-sym (last iter))
         (i-var (gensym dolist-i))
         (iter-list (gensym dolist-list)))
    `(let* ((~iter-list ~list-value-sym)
			 (~i-var 0))
       ;; FIXME: use real while for block support
       (__while (< ~i-var (length ~iter-list))
		 (~let-sym ((~var-sym (nth ~i-var ~iter-list)))
           ~@body)
         (set ~i-var (+ ~i-var 1))))))

(defun wrap-apply (f)
  (fn (args) (apply f args)))

(defun map (f l)
  (assert (not (__is nil l)) "map: list is None")
  (let* ((n (length l))
         (r (cons:alloc n))
         (i 0))
    (__while (< i n)
     ;; avoid setf
     (cons:list-set r i (f (nth i l)))
     (set i (+ i 1)))
    r))

(defun map-apply (f l) (map (wrap-apply f) l))

(defun fold (f start l)
  (let* ((r start))
	(dolist (e l)
	  (set r (f r e)))
	r))

(defun foldr (f start l)
  (let* ((r start))
	(dolist (e (reversed l))
	  (set r (f e r)))
	r))

(defun range (&rest args)
  (let* ((nargs (length args))
         (iend 0)
         (istart 0)
         (step 1))
    (cond ((eq nargs 1)
           (set iend (head args)))
          ((eq nargs 2)
           (setf (:= istart iend) args))
          ((eq nargs 3)
           (setf (:= istart iend step) args)))
    (assert (and (int? iend) (>= iend 0)) iend)
    (assert (and (int? istart) (>= istart 0)) istart)
    (assert (and (int? step) (>= step 0)) step)
    (let* ((n (/  (- iend istart) step))
           (r (cons:alloc n))
           (i 0))
      (__while (< istart iend)
               (cons:list-set r i istart)
               (+= i 1)
               (+= istart step))
      r)))

;; TODO: generator
(defun enumerate (l)
  (let* ((i 0)) (map (fn (e)
                         (let* ((oldi i))
                           (+= i 1)
                           (list oldi e)))
                     l)))

(defun zip (&rest ls)
  (let* ((n (apply min (map length ls)))
         (r (cons:alloc n)))
    (dolist (iel (range n))
      (cons:list-set r iel
                     (map (fn (l) (nth iel l)) ls))
      (assert (< iel n) (list iel n))
      )
    r))

;; TODO: test
(defun curry (f &rest fixed-args) (fn (&rest args) (apply f (extend fixed-args args))))

(defun destructuring-bind-parse (target value-evaluated-form)
  (defun symbol-form (target) `((~target ~value-evaluated-form)))
  (cond
   ((symbol? target)
    (symbol-form target))
   ((list? target)
    (fold
     (fn (binds target)
         (assert (eq (length target) 2))
         (let* ((itarget (head target))
                (target (last target)))
           (assert (int? itarget))
           ;; TODO assert when value was not exhausted
           ;; fixme + doesn't work for cons
           (cons:append binds
              (destructuring-bind-parse target `(nth ~itarget ~value-evaluated-form)))))
     (list)
     (enumerate target)))
   (true (assert false (+ "unknown destructuring " (repr target))))))

(defmacro setf (target value)
  (let* ((value-var (gensym setf-value)))
    `(progn
       ;; avoid let's sub-env
       (def ~value-var ~value)
       ~@(cond
          ((symbol? target)
           `((set ~target ~value-var)))
          ;; tuples
          ((named-operator? target ':=)
           (map-apply
            (fn (var val)
                (assert (symbol? var) (repr var))
                (list 'set var val))
            (destructuring-bind-parse (as-list (slice target 1 nil))
                                      value-var)))
          ((named-operator? target 'aref)
           (let* ((target (cdr target)))
             (assert (eq (length target) 2) (repr target))
             (let* ((obj (1st target))
                    (key (2nd target)))
               (when (keyword? key)
                 (set key (keyword-name key)))
               `(
                 (cond
                  ((list? ~obj)
                   (progn
                     (assert (num? ~key) (repr ~key))
                     (cons:list-set ~obj ~key ~value-var)))
                  ((vec? ~obj)
                   (vec-set ~obj ~key ~value-var))
                  (true
                   (assert (dict? ~obj) (repr ~obj))
                   (dict-set ~obj ~key ~value-var)))
                 ))))
          (true
           (assert false (+ "unknown target" (repr target))))))))

(defmacro let (vars &rest body)
  (let* ((var-defs
          (fold
           (fn (vars target-value)
               (assert (eq (length target-value) 2) target-value)
               (let* ((target (head target-value))
                      (value (last target-value)))
                 (cons:append vars
                    (if (named-operator? target ':=)
                        (let* ((value-var (gensym let-value)))
                          (cons (list value-var value)
                                (destructuring-bind-parse (as-list (slice target 1 nil)) value-var)))
                      (list target-value)))))
           (list) vars)))
    `(progn
                 (let*
                     (~@(map-apply
                         (fn (var val)
                             (list var val))
                         var-defs))
                   ~@body))))

(defmacro cond (&rest clauses)
  ;; work from the back outwards to topmost if
  (foldr
   (fn (clause ifs)
	   (let* ((test (car clause))
              (body (cdr clause)))
         (if (__is test 'true)
             ;; else/true branch skips test
             (progn
               (assert (__is ifs nil) "else/true needs to be the last clause")
               `(progn ~@body))
		   (if ifs
			   `(if ~test (progn ~@body) ~ifs)
		     `(when ~test (progn ~@body))))))
   nil
   clauses))

(defun member? (e l) (contains? l e))
(defun not-member? (e l) (not  (member? e l)))

(defun reversed (l)
  (let* ((n (length l))
         (r (cons:alloc n))
         (i 0))
    (__while
     (< i n)
     (let*
         ((e (nth i l)))
       ;; avoid setf
       (cons:list-set r (- n 1 i) e))
     (set i (+ i 1)))
    r))


(defun head (l) (nth 0 l))
;;(defun tail (l) (nth (- (length l) 1) l))
(defun 1st (l) (nth 0 l))
(defun 2nd (l) (nth 1 l))
(defun 3rd (l) (nth 2 l))
(defun 4th (l) (nth 3 l))
(defun 5th (l) (nth 4 l))
(defun last (l) (nth (- (length l) 1) l))

(defun cadr (l) (car (cdr l)))

(defun cons:list-set (list index value)
  (assert (list? list))
  (assert (int? index))
  (if (eq index 0)
      (setcar list value)
    (cons:list-set (cdr list) (- index 1) value)))

(defun neq (x) (not (eq x)))

(defun min (a &rest args)
  (let* ((mi a))
	(dolist (x args)
	  (when (< x mi)
		(set mi x)))
	mi))

(defun max (ma &rest args)
  (dolist (x args)
	(when (> x ma)
	  (set ma x)))
  ma)

;; TODO support 1 as default
(defmacro def-setter-op-varargs (name op)
  `(defmacro ~name (var &rest args)
	 `(setf ~~var (apply ~op (list ~~var ~~@args)))))
;; (defmacro += (var &rest args)  `(setf ~var (apply + (list ~var ~@(eval args)))))
(def-setter-op-varargs += +)
(def-setter-op-varargs -= -)
(def-setter-op-varargs *= *)
(def-setter-op-varargs /= /)

(defun test-name (what) (+ what "?"))

(defmacro isinstance (value type)
  (list (intern (test-name (symbol-name type)))
		value))

(defun all (l) (dolist (e l) (unless e (return false)) true))
(defun is (a &rest bs) (if (eq (length bs) 1)
                           (__is a (1st bs))
                           (all (map (fn (b) (is a b)) bs))))
(defun is-not (a b) (not (__is a b)))

(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))
