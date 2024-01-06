(requires "testing.lisp")

(test-equal (property 'foo 'bar "OK1") "OK1")

(set-property "OK2" 'foo 'bar)
(test-equal (property 'foo 'bar "FAIL2") "OK2")

(setf (property 'foo 'bar) "OK3")
(test-equal (property 'foo 'bar "FAIL3") "OK3")

(setf (property 'foo 'bar) "OK4")
(test-equal (remove-property 'foo 'bar) "OK4")
(test-equal (property 'foo 'bar "OK5") "OK5")

(test-equal (eql 'a 'a) t)
(test-equal (eql 'a 'c) nil)

(format-object (standard-output) "symbol.lisp end" nil)
(finish-output (standard-output))
