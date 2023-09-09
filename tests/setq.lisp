(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(let ((foo 1))
    (setq foo 2)
    (print foo)
    (setf foo 3)
    (print foo))