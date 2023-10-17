(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(defclass <foo> ()
    ((bar :reader get-bar :writer set-bar :initarg bar)))

(let ((f (create (class <foo>) 'bar 1)))
  (print (get-bar f))
  (set-bar f 2)
  (print (get-bar f)))

;; check inheritance
(defclass <foo2> (<foo>) ())
(let ((f (create (class <foo2>) 'bar 2)))
  (print (get-bar f)))

;; check initialize-object
(defclass <foo3> () ((bar3 :reader get-bar3 :writer set-bar3 :initarg bar)))
(defmethod initialize-object ((f <foo3>) :rest args)
    (set-bar3 f 3)
    (call-next-method))
(print (get-bar3 (create (class <foo3>) 'bar 0)))