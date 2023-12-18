(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(if (char= #\a #\a)
    (print "OK1")
    (print "FAIL1"))

(if (char<= #\a #\a)
    (print "OK2")
    (print "FAIL2"))

(if (char<= #\a #\b)
    (print "OK3")
    (print "FAIL3"))

(if (char>= #\a #\a)
    (print "OK4")
    (print "FAIL4"))

(if (char>= #\c #\b)
    (print "OK5")
    (print "FAIL5"))

(if (char< #\a #\b)
    (print "OK6")
    (print "FAIL6"))

(if (char< #\a #\a)
    (print "FAIL7")
    (print "OK7"))

(if (char> #\b #\a)
    (print "OK8")
    (print "FAIL8"))

(if (char> #\a #\a)
    (print "FAIL9")
    (print "OK9"))

(if (char/= #\a #\b)
    (print "OK10")
    (print "FAIL10"))

(if (char/= #\a #\a)
    (print "FAIL11")
    (print "OK11"))
