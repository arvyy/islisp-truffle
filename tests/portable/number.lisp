(requires "testing.lisp")

(test-equal (numberp 1) t)
(test-equal (numberp "1") nil)

(test-equal (= 1 (min 1)) t)
(test-equal (= 1 (min 3 1)) t)
(test-equal (= 1 (min 1 3)) t)

(test-equal (= 1 (max 1)) t)
(test-equal (= 3 (max 3 1)) t)
(test-equal (= 3 (max 1 3)) t)

(format-object (standard-output) "number.lisp end" nil)
(finish-output (standard-output))
