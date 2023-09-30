(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(print
    (+ 1
       (catch 'foo
         (+ 2 (throw 'foo 1)))
       3))