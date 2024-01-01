(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(print (elt #(1 2 3) 0))
(print (elt '(4 5 6) 0))
(format-char (standard-output) (elt "789" 0))
(format-char (standard-output) #\newline)
(finish-output (standard-output))
