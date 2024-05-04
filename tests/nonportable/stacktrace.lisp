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
(print "")

;;;;;; no next method
(defgeneric generic-method (value))
(defmethod generic-method ((value <number>))
    (call-next-method))
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (generic-method 1)
        (print "FAIL")))
(print "")

;;;;;; truffle interop error
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (let ((js-obj (eval "js" "({a: 1})")))
          (truffle-object-field js-obj "b"))
        (print "FAIL")))
(print "")

(finish-output (standard-output))
