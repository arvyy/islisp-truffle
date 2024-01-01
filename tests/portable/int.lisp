(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(print #b10)
(print #b-10)

(print #o10)
(print #o-10)

(print 10)
(print -10)

(print #x1f)
(print #x-1f)
(finish-output (standard-output))
