(requires "testing.lisp")

(test-equal (characterp #\a) t)
(test-equal (characterp "a") nil)

(test-equal (char= #\a #\a) t)
(test-equal (char<= #\a #\a) t)
(test-equal (char<= #\a #\b) t)
(test-equal (char>= #\a #\a) t)
(test-equal (char>= #\c #\b) t)
(test-equal (char< #\a #\b) t)
(test-equal (char< #\a #\a) nil)
(test-equal (char> #\b #\a) t)
(test-equal (char> #\a #\a) nil)
(test-equal (char/= #\a #\b) t)
(test-equal (char/= #\a #\a) nil)

(format (standard-output) "char.lisp end")
(finish-output (standard-output))
