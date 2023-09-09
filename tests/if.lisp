(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))