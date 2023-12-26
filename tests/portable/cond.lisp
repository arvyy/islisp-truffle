(requires "testing.lisp")

(test-equal
    (cond)
    nil)

(test-equal
    (cond ("1"))
    "1")

(test-equal
    (cond ((> 2 1) "1"))
    "1")

(test-equal
    (cond ((> 2 1) "1" "2"))
    "2")

(test-equal
    (cond
        ((< 2 1) "1")
        ((> 2 1) "2")
        ((> 3 1) "3"))
    "2")

(format (standard-output) "cond.lisp end")
