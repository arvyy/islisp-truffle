(requires "testing.lisp")

(let ((foo 1))
    (setq foo 2)
    (test-equal foo 2)
    (setf foo 3)
    (test-equal foo 3))

(defglobal *global* 1)
(setq *global* 2)
(test-equal *global* 2)

(format (standard-output) "end setq.lisp")
(finish-output (standard-output))
