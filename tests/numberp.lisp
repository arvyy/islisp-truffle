(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(print (if (numberp 1) "OK1" "FAIL1"))
(print (if (numberp "1") "FAIL2" "OK2"))
