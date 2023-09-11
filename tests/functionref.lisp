(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(print (funcall #'+ 1 2))
(print (funcall (function +) 1 2))
