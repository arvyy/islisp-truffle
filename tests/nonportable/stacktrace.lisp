(requires "builtin/truffle.lisp")

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

;;;;;; undefined class error
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (class <doesntexist>)
        (print "FAIL")))
(print "")

;;;;;; undefined conversion error
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (convert #\A <list>)
        (print "FAIL")))
(print "")

;;;;;; wrong arg count
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (let ((fn (lambda (a) a)))
          (funcall fn 1 2))
        (print "FAIL")))
(print "")

;;;;;; immutable binding error
(defconstant shouldbeimmutable "foo")
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (setq shouldbeimmutable "bar")
        (print "FAIL")))
(print "")

;;;;; plain `error` call
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (error "Error with value: ~A" "test")
        (print "FAIL")))
(print "")

;;;;; domain-error
(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        (+ 1 "1")
        (print "FAIL")))
(print "")


(finish-output (standard-output))
