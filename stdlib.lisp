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

(defun map (f l)
  (let ((r (list)))
	(dolist (e l)
	  (+= r (list (f e))))))
(defmacro setf (target value)
  (cond ((symbolp target)
         (list 'set target value))
        ((tuplep target)
         (let ((value-var (gensym))
               (clauses (map
                         (fn (e)
                             (let ((i (head e))
                                   (v (last e)))
                               (list 'set v (list 'nth i value-var)))
                             (enumerate target)))))
           (extend (list 'let (list (list value-var value)))
                   clauses)))
		
        (true (raise (Exception "unknown target")))))


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

;; TODO support 1 as default
(defmacro def-setter-op-varargs (name op)
  `(defmacro ~name (var &rest args)
	 `(setf ~~var (apply ~op (list ~~var ~~@args)))))
(def-setter-op-varargs += +)
(def-setter-op-varargs -= -)
(def-setter-op-varargs *= *)
(def-setter-op-varargs /= /)


