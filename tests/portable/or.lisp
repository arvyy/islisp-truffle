(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(print (if (or (= 2 2) (> 2 1)) "OK1" "FAIL1"))
(print (if (or (= 2 2) (> 1 2)) "OK2" "FAIL2"))
(finish-output (standard-output))
