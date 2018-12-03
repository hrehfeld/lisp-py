(defmacro when (test &rest body)
  `(if ~test (progn ~@body)))

(defmacro dolist (iter &rest body)
  (assert (eq (length iter) 2))
  (let ((var (head iter))
        (l (last iter))
        (i-var (gensym))
        (iter-list (gensym)))
    `(let ((~iter-list ~l)
           (~i-var 0))
       (while (< ~i-var (length ~iter-list))
         (let ((~var (nth ~i-var ~iter-list)))
           ~@body
           (set ~i-var (+ ~i-var 1)))))))

(defun wrap-apply (f)
  (fn (&rest args) (apply f args)))

(defun map (f l)
  (let ((r (list)))
	(dolist (e l)
	  (+= r (list (f e))))))

;; TODO: generator
(defun enumerate (l)
  (let ((r (list))
		(i 0))
	(dolist (e l)
	  (+= r (list (tuple i e)))
	  (+= i 1))))

(defun zip (a b)
  (let ((n (min (map length (list a b))))
		(a (slice a n))
		(b (slice b n)))
	(map
	 (wrap-apply  (fn (i el-a) (tuple el-a (nth i b))))
	 (enumerate a))))

; TODO: test
(defun curry (f &rest fixed-args) (fn (&rest args) (apply f (extend fixed-args args))))

(defmacro setf (target value)
  (cond ((symbolp target)
         `(set ~target ~value))
		;; lists
		((listp target)
		 (let ((values (eval value)))
		   (assert (eq (length target) (length values))
				   (+ "\n" (repr target) "\n" (repr values) "\n" (repr value)))
		   (dolist (t (enumerate target))
			 (apply
			  (fn (i t)
				  (setf t (nth values i)))
			  t))))
        (true
		 (princ target)
		 (throw (Exception (+ "unknown target" (repr target)))))))


(defmacro cond (&rest clauses)
  (assert (>= (length clauses) 1) "cond not allowed with only one clause")
  (let ((ifs nil))
    (dolist (clause (reversed clauses))
      (let ((test (car clause))
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
  (let ((r (list))
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

(defun min (&rest args)
  (let ((mi (head args)))
	(dolist (x (tail args))
	  (when (< x mi)
		(set mi x)))))

(defun max (&rest args)
  (let ((ma (head args)))
	(dolist (x (tail args))
	  (when (> x ma)
		(set ma x)))))

;; TODO support 1 as default
(defmacro def-setter-op-varargs (name op)
  `(defmacro ~name (var &rest args)
	 `(setf ~~var (apply ~op (list ~~var ~~@args)))))
;; (defmacro += (var &rest args)  `(setf ~var (apply + (list ~var ~@(eval args)))))
(def-setter-op-varargs += +)
(def-setter-op-varargs -= -)
(def-setter-op-varargs *= *)
(def-setter-op-varargs /= /)


