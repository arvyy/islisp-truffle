(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(print (if (and (= 2 2) (> 2 1)) "OK1" "FAIL1"))
(print (if (and (= 2 2) (> 1 2)) "FAIL2" "OK2"))
(finish-output (standard-output))
