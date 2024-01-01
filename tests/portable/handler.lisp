(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(if (ignore-errors)
    (print "FAIL1")
    (print "OK1"))

(if (ignore-errors
      (print "OK2.1")
      (+ 1 'b)
      (print "FAIL2"))
    (print "FAIL2")
    (print "OK2.2"))
(finish-output (standard-output))
