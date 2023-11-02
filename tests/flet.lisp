(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(flet ((f (x) (+ x 3)))
    (flet ((f (x) (+ x (f x))))
        (if (= (f 7) 17)
            (print "OK1")
            (print "FAIL1"))))