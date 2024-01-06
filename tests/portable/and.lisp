(requires "testing.lisp")

(test-equal (and (= 2 2) (> 2 1)) t)
(test-equal (and (= 2 2) (> 1 2)) nil)

(format-object (standard-output) "and.lisp end" nil)
(finish-output (standard-output))
