(requires "testing.lisp")

(flet ((f (x) (+ x 3)))
    (flet ((f (x) (+ x (f x))))
        (test-equal (= (f 7) 17) t)))

(format (standard-output) "flet.lisp end")
(finish-output (standard-output))
