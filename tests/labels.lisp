(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(labels ((evenp (n)
           (if (= n 0)
               t
               (oddp (- n 1))))
         (oddp (n)
           (if (= n 0)
               nil
               (evenp (- n 1)))))
    (if (evenp 88)
        (print "OK1")
        (print "FAIL1")))