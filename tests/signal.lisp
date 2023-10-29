(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(block exit
    (with-handler
        (lambda (c)
            (print "FAIL1")
            (return-from exit nil))
        (print "OK1")))

(block exit
    (with-handler
        (lambda (c)
            (print "OK2")
            (return-from exit nil))
        (error "message")
        (print "FAIL2")))

(block exit
    (with-handler
        (lambda (c)
            (print "OK3.1")
            (continue-condition c "OK3.2")
            (print "FAIL3"))
        (let ((v (signal-condition (create (class <simple-error>) 'format-string "message") t)))
            (print v))))