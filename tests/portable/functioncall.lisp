(requires "testing.lisp")

(test-equal (+ 1 2) 3)
(test-equal (funcall #'+ 1 2) 3)
(test-equal (funcall (function +) 1 2) 3)
(test-equal (apply #'+ (list 1 2)) 3)
(test-equal (apply #'+ 1 (list 2)) 3)
(test-equal ((lambda (a) (+ 1 a)) 2) 3)

(format (standard-output) "functioncall.lisp end")
(finish-output (standard-output))