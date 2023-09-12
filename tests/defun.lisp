(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(defun foo1 (a) a)
(print (foo1 1))

(defun foo2 (a :rest b)
    (car b))

(print (foo2 1 2))

(defun foo3 (a &rest b)
    (car b))

(print (foo3 1 3))

(defun foo4 ()
    4)

(print (foo4))