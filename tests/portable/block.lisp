(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(defun foo ()
    (block x
        (+ 10 (return-from x 6) 22)))

(print (foo))