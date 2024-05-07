(requires "testing.lisp")

(defun foo ()
    `(1 2 ,@(progn 1)))

(block exit
    (with-handler
        (lambda (condition)
            (test-equal (instancep condition (class <domain-error>)) t)
            (return-from exit nil))
        (foo)
        (print "FAIL")))

(format (standard-output) "quasiquote.lisp end")
(finish-output (standard-output))