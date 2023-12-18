(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

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