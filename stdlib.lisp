(defmacro dotimes (iter &rest body)
  (assert (eq (length iter) 2))
  (let ((var (head iter))
        (l (last iter))
        (i-var (gensym)))
    (list 'let (list (list i-var 0))
          (list 'while (list '< i-var (list 'length l))
                (list 'let (list (list var (list 'nth i-var l)))
                      (extend (list 'progn)
                              body
                              (list (list 'set i-var (list '+ i-var 1)))
                              ))))))
;; (for x in mylist (progn (print x)))
;; (let ((var 0))
;;   (while (< var (length l))
;;     ,@body))

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
        (else (raise (Exception)))))


(defmacro cond (&rest clauses)
  (let ((ifs '()))
    (dotimes (clause (reversed clauses))
      (let ((test (head clause))
            (body (list 'progn (tail clause)))
            (oldifs ifs))
        (set ifs (if (is test 'else)
                     (progn
                       (assert (not ifs))
                       body)
                   (list 'if test body)))
        (extend ifs oldifs))))
  ifs)
