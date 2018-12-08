(defmacro when (test &rest body)
  `(if ~test (progn ~@body)))

(defmacro dolist (iter &rest body)
  (assert (eq (length iter) 2))
  (let* ((var-sym (head iter))
         (list-value-sym (last iter))
         (i-var (gensym i-var))
         (iter-list (gensym iter-list)))
    `(let* ((~iter-list ~list-value-sym)
			(~i-var 0))
	   (assert (not (is nil ~iter-list)) "list is None")
       (while (< ~i-var (length ~iter-list))
		 (let* ((~var-sym (nth ~i-var ~iter-list)))
           ~@body)
         (set ~i-var (+ ~i-var 1))))))

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
  (let* ((r start))
	(dolist (e l)
	  (set r (f r e)))
	r))

(defun range (&rest args)
  (let* ((m (length args))
		 (n 0)
		 (i 0)
		 (step 1))
	(cond ((eq m 1)
		   (set n (head args)))
		  ((eq m 2)
		   (setf (tuple i n) args))
		  ((eq m 3)
		   (setf (tuple i n step) args)))
	(assert (and (int? n) (>= n 0)) n)
	(assert (and (int? i) (>= i 0)) i)
	(assert (and (int? step) (>= step 0)) step)
	(let* ((l (list)))
	  (while (< i n)
		(append l i)
		(+= i step))
	  l)))

;; TODO: generator
(defun enumerate (l)
  (let* ((i 0)) (map (fn (e)
						(let* ((oldi i))
						  (+= i 1)
						  (list oldi e)))
					l)))

(defun zip (&rest ls)
  (let* ((n (apply min (map length ls)))
		 (r (list)))
	(dolist (iel (range n))
	  (append r (map (fn (l) (nth iel l)) ls))
	  (assert (< iel n) (list iel n))
	  )
	  r))

; TODO: test
(defun curry (f &rest fixed-args) (fn (&rest args) (apply f (extend fixed-args args))))

(defun destructuring-bind-parse (target value-evaluated-form)
  (if (symbol? target)
	  `((~target ~value-evaluated-form))
	(fold
	 (fn (binds targ)
		 (let* ((itarg (head targ))
				(targ (last targ)))
		   (assert (int? itarg))
		   ;; TODO assert when value was not exhausted
		   (+ binds
			  (destructuring-bind-parse targ `(nth ~itarg ~value-evaluated-form)))))
	 (list)
	 (enumerate target))))

(defmacro setf (target value)
  (let* ((value-var (gensym value)))
	`(progn
	   (def ~value-var ~value)
	   ~@(cond
		  ((symbolp target)
		   `((set ~target ~value-var)))
		  ;; tuples
		  ((named-operator? target 'tuple)
		   (map-apply
			(fn (var val)
				(assert (symbol? var) (repr var))
				(list 'set var val))
			(destructuring-bind-parse (as-list (slice target 1 nil))
									  value-var)))
		  ((named-operator? target 'aref)
		   (let* ((target (tail target)))
			 (assert (eq (length target) 2) (repr target))
			 (let* ((obj (1st target))
					(key (2nd target)))
			   (assert (symbol? obj) (repr obj))
			   (assert (or (symbol? key) (num? key)) (repr key))
			   `(
				 (if (list? ~obj)
					 (progn
					   (assert (num? ~key) (repr ~key))
					   (list-set ~obj ~key ~value-var))
				   (assert (dict? ~obj) (repr ~obj))
				   (assert (str? ~key) (+ ~key " " (repr (quote ~target))))
				   (dict-set ~obj (keyword ~key) ~value-var))
				 ))))
		  (true
		   (princ target)
		   (throw (Exception (+ "unknown target" (repr target)))))))))

(defmacro let (vars &rest body)
  ;; TODO use fold
  (let* ((var-defs
		  (fold
		   (fn (vars target-value)
			   (let* ((target (head target-value))
					  (value (last target-value))
					  (value-var (gensym value))
					  (r (destructuring-bind-parse target value-var)))
				 (+ vars `((~value-var ~value)) r)
				 ))
		   (list) vars)))
	(let* ((r `(progn
				 (let*
					 (~@(map-apply
						 (fn (var val)
							 (list var val))
						 var-defs))
						~@body))))
	  r)))

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
(defun 1st (l) (nth 0 l))
(defun 2nd (l) (nth 1 l))
(defun 3rd (l) (nth 2 l))
(defun 4th (l) (nth 3 l))
(defun 5th (l) (nth 4 l))
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
