(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(defgeneric foo1 (a b))
(defmethod foo1 :before (a b) (print 2))
(defmethod foo1 :before ((a <integer>) (b <integer>)) (print 1))
(defmethod foo1 (a b) (print 3))
(defmethod foo1 ((a <integer>) (b <integer>))
    (call-next-method)
    (print 4))
(defmethod foo1 :after (a b) (print 5))
(defmethod foo1 :after ((a <integer>) (b <integer>)) (print 6))
(defmethod foo1 :around (a b)
    (call-next-method)
    (print 7))
(defmethod foo1 :around ((a <integer>) (b <integer>))
    (call-next-method)
    (print 8)
    a)

(let ((result (foo1 9 10)))
  (print result))


;; test setf
(defgeneric (setf foo2) (value a))
(defmethod (setf foo2) (value a)
    (print a)
    (print value))

(setf (foo2 10) 11)

(format (standard-output) "defgeneric.lisp end")
(finish-output (standard-output))
