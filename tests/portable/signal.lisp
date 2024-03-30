(defun test-print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(block exit
    (with-handler
        (lambda (c)
            (test-print "FAIL1")
            (return-from exit nil))
        (test-print "OK1")))

(block exit
    (with-handler
        (lambda (c)
            (test-print "OK2")
            (return-from exit nil))
        (error "message")
        (test-print "FAIL2")))

(block exit
    (with-handler
        (lambda (c)
            (test-print "OK3.1")
            (continue-condition c "OK3.2")
            (test-print "FAIL3"))
        (let ((v (signal-condition (create (class <simple-error>) 'format-string "message") t)))
            (test-print v))))
(finish-output (standard-output))
