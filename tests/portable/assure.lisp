(requires "testing.lisp")

(test-equal (assure <integer> (+ 1 9)) 10)
(test-equal (assure <number> (+ 1 9)) 10)
(block exit
    (with-handler
        (lambda (c)
            (return-from exit 1))
        (assure <number> "string")
        (format (standard-output) "FAIL")))

(test-equal (the <integer> (+ 1 9)) 10)
(test-equal (the <number> (+ 1 9)) 10)
(block exit
    (with-handler
        (lambda (c)
            (return-from exit 1))
        (the <number> "string")
        (format (standard-output) "FAIL")))

(format (standard-output) "assure.lisp end")