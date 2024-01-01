(requires "testing.lisp")

(let ((foo 1))
    (setq foo 2)
    (test-equal foo 2)
    (setf foo 3)
    (test-equal foo 3))

(format (standard-output) "end setq.lisp")
(finish-output (standard-output))
