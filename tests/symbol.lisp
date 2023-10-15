(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(print (property 'foo 'bar "OK1"))

(set-property "OK2" 'foo 'bar)
(print (property 'foo 'bar "FAIL2"))

(setf (property 'foo 'bar) "OK3")
(print (property 'foo 'bar "FAIL3"))

(setf (property 'foo 'bar) "OK4")
(print (remove-property 'foo 'bar))
(print (property 'foo 'bar "OK5"))
