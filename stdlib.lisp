(defmacro when (test &rest body)
  `(if ~test (progn ~@body)))

(defmacro dolist (iter &rest body)
  (assert (eq (length iter) 2))
  (let* ((var (head iter))
        (l (last iter))
        (i-var (gensym i-var))
        (iter-list (gensym iter-list)))
    `(let* ((~iter-list ~l)
           (~i-var 0))
	   (assert (not (is nil ~iter-list)) "list is None")
       (while (< ~i-var (length ~iter-list))
           ~@body
           (set ~i-var (+ ~i-var 1)))))))
         (let* ((~var (nth ~i-var ~iter-list)))

(defun wrap-apply (f)
  (fn (args) (apply f args)))

(defun map (f l)
  (assert (not (is nil l)) "map: list is None")
  (let* ((r (list)))
	(dolist (e l)
	  (append r (f e)))
	(assert (not (null? r)))
	r))

(defun map-apply (f l) (map (wrap-apply f) l))

(defun fold (f start l)
  (print f start l)
  (let* ((r start))
	(dolist (e l)
	  (print e l)
	  (set r (f r e)))
	r))

(defun range (n)
  (assert (int? n) n)
  (assert (>= n 0) n)
  (let* ((i 0) (l '()))
	(while (< i n)
	  (append l i)
	  (+= i 1))
	l))

;; TODO: generator
(defun enumerate (l)
  (let* ((i 0)) (map (fn (e)
						(let* ((oldi i))
						  (+= i 1)
						  (list oldi e)))
					l)))

(defun zip (&rest ls)
  (let* ((n (apply min (map length ls)))
		 (r '()))
	(dolist (iel (range n))
	  (append r (map (fn (l) (nth iel l)) ls))
	  (print "%%%%%%%%%%%%%%%%%%%%%%%%" (repr r))
	  )
	  r))

; TODO: test
(defun curry (f &rest fixed-args) (fn (&rest args) (apply f (extend fixed-args args))))

(defmacro setf (target value)
  (cond ((symbolp target)
         `(set ~target ~value))
		;; tuples
		((named-operator? target 'tuple)
		 `(progn
			~@(let* ((values value)
					(targets (as-list (slice target 1 nil)))
					(num-targets (-  (length target) 1)))
				(assert (eq num-targets (length values))
						(+ "\n" (repr target) "\n" (repr values) "\n" (repr value)))
				(map-apply
				 (fn (targ value)
					 (assert (symbolp targ))
					 ;; generate list of setfs
					 (list 'setf targ value))
				 (zip targets values)))))
        (true
		 (princ target)
		 (throw (Exception (+ "unknown target" (repr target)))))))


(defmacro cond (&rest clauses)
  (assert (>= (length clauses) 1) "cond not allowed with only one clause")
  (let* ((ifs nil))
    (dolist (clause (reversed clauses))
      (let* ((test (car clause))
            (body (cdr clause)))
        (set ifs (if (eq test 'true)
                    ;; else/true branch skips test
                     (progn
                       (assert (not ifs) "else/true needs to be the last clause")
                       `(progn ~@body))
                   `(if ~test (progn ~@body) ~ifs)))))
    ifs))


(defmacro member (e l) `(contains? ~l ~e))

(defun break (&rest values)
  (assert (<= (length values) 1))
  (return-from nil (when values (head values))))

(defun reversed (l)
  (let* ((r (list))
        (i (- (length l) 1)))
    (while (>= i 0)
      (append r (nth i l))
      (set i (- i 1)))
    r))


(defun head (l) (nth 0 l))
(defun last (l) (nth (- (length l) 1) l))

(defun car (l) (head l))
(defun cdr (l) (tail l))
(defun cadr (l) (car (tail l)))

(defun min (a &rest args)
  (let* ((mi a))
	(dolist (x args)
	  (when (< x mi)
		(set mi x)))
	mi))

(defun max (&rest args)
  (let* ((ma (head args)))
	(dolist (x (tail args))
	  (when (> x ma)
		(set ma x)))
	ma))

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
