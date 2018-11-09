;; symbols

(def symbols (dict))

(make-struct Symbol name)

(defun intern (s & package)
  (dict-setdefault symbols s (make-Symbol s))
  (dict-get symbols s))

(defun symbol-name (sym) (Symbol-name sym))


;; main

