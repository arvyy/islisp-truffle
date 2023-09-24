(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(defclass <foo> ()
    ((bar :reader get-bar :writer set-bar :initarg bar)))

(let ((f (create (class <foo>) 'bar 1)))
  (print (get-bar f))
  (set-bar f 2)
  (print (get-bar f)))