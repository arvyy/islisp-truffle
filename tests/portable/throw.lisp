(requires "testing.lisp")

(test-equal
    (+ 1
       (catch 'foo
         (+ 2 (throw 'foo 1)))
       3)
    5)

(format-object (standard-output) "throw.lisp end" nil)
(finish-output (standard-output))
