(requires "testing.lisp")

(defglobal *data* '())

(let ((i 3))
  (while (> i 0)
    (setq *data* (cons i *data*))
    (setf i (- i 1))))

(test-equal *data* '(1 2 3))

(format-object (standard-output) "while.lisp end" nil)
(finish-output (standard-output))
