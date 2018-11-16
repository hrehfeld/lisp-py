(defmacro for (var in l body)
  (list 'let (list (list var 0))
        (list 'while (list '< var (list 'length l))
              body)))
;; (for x in mylist (progn (print x)))
;; (let ((var 0))
;;   (while (< var (length l))
;;     ,@body))