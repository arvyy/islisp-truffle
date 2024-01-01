(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(print (length "ab"))
(print (length '(1 2)))
(print (length #(1 2)))
(finish-output (standard-output))
