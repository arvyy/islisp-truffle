(requires "testing.lisp")

(test-equal (or (= 2 2) (> 2 1)) t)
(test-equal (or (= 2 2) (> 1 2)) t)

(format-object (standard-output) "or.lisp end" nil)
(finish-output (standard-output))