(requires "testing.lisp")

(test-equal (ignore-errors) nil)
(test-equal
    (ignore-errors
      (format (standard-output) "OK~%")
      (+ 1 'b)
      (print "FAIL"))
    nil)

(format (standard-output) "handler.lisp end")
(finish-output (standard-output))
