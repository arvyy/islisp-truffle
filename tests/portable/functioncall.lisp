(requires "testing.lisp")

(test-equal (+ 1 2) 3)
(test-equal (funcall #'+ 1 2) 3)
(test-equal (funcall (function +) 1 2) 3)
(test-equal (apply #'+ (list 1 2)) 3)
(test-equal (apply #'+ 1 (list 2)) 3)
(test-equal ((lambda (a) (+ 1 a)) 2) 3)

(block exit
    (with-handler
        (lambda (condition)
            (test-equal t (instancep condition (class <undefined-function>)))
            (return-from exit nil))
        (doesntexist)
        (test-equal 1 0)))

(block exit
    (with-handler
        (lambda (condition)
            (test-equal t (instancep condition (class <undefined-function>)))
            (return-from exit nil))
        #'doesntexist
        (test-equal 1 0)))

(format (standard-output) "functioncall.lisp end")
(finish-output (standard-output))