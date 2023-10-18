(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(if (= 3 (+ 1 2))
    (print "OK1")
    (print "FAIL1"))

(if (= 3 (+ 1.0 2.0))
    (print "OK2")
    (print "FAIL2"))

(block exit
    (with-handler
      (lambda (condition)
        (if (instancep condition (class <domain-error>))
            (print "OK3")
            (print "FAIL3"))
        (return-from exit nil))
      (+ 1 "2")
      (print "FAIL3")))