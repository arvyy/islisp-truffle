(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(if (= 3 (+ 1 2))
    (print "OK1")
    (print "FAIL1"))

(block exit
    (with-handler
      (lambda (condition)
        (if (instancep condition (class <domain-error>))
            (print "OK2")
            (print "FAIL2"))
        (return-from exit nil))
      (+ 1 "2")
      (print "FAIL2")))