(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(let ((i 3))
  (while (> i 0)
    (print i)
    (setf i (- i 1))))