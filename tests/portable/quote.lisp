(requires "testing.lisp")

(test-equal '(1 . 2) (cons 1 2))

(format-object (standard-output) "quote.lisp end" nil)