(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(defun foo1 (a) a)
(print (foo1 "OK1"))

(defun foo2 (a :rest b)
    (car b))

(print (foo2 "FAIL2" "OK2"))

(defun foo3 (a &rest b)
    (car b))

(print (foo3 "FAIL3" "OK3"))

(defun foo4 ()
    "OK4")

(print (foo4))

;; test conditions are properly raised on bad arg count
(block exit
    (with-handler
        (lambda (condition)
            (if (instancep condition (class <program-error>))
                (print "OK5")
                (print "FAIL5"))
            (return-from exit nil))
        (foo1)
        (print "FAIL5")))
(finish-output (standard-output))
