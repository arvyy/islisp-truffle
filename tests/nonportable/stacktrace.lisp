(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

;;;;; undefined variable
(defun f1 ()
    (+ 1
       (f2)))

(defun f2 ()
    (+ 3 unbound-var))

(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (f1)
        (print "FAIL")))
(print "")

;;;;;;; division by zero
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (quotient 1 0)
        (print "FAIL")))
(print "")

;;;;;;; index out of bounds
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (elt '(1 2 3) 4)
        (print "FAIL")))

(finish-output (standard-output))
