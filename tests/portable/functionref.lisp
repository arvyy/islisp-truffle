(requires "testing.lisp")

(test-equal (funcall #'+ 1 2) 3)
(test-equal (funcall (function +) 1 2) 3)

(format (standard-output) "end functionref.lisp")
(finish-output (standard-output))
