(requires "testing.lisp")

(test-equal (floatp 1) nil)
(test-equal (floatp "1.0") nil)
(test-equal (floatp 1.0) t)

(test-equal (instancep (float 1) (class <float>)) t)
(test-equal (instancep (float 1000000000000) (class <float>)) t)
(block exit
    (with-handler
        (lambda (condition)
            (test-equal t (instancep condition (class <domain-error>)))
            (return-from exit nil))
        (float "1")
        (test-equal 1 0)))

(test-equal (floatp *most-positive-float*) t)
(test-equal (floatp *most-negative-float*) t)

(format (standard-output) "float.lisp end")
(finish-output (standard-output))
