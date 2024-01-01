(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(defclass <foo> ()
    ((bar :reader get-bar :writer set-bar :accessor bar :initarg bar :boundp bar-boundp)))

(let ((f (create (class <foo>) 'bar 1)))
  (print (get-bar f))
  (set-bar 2 f)
  (print (get-bar f)))

;; check inheritance
(defclass <foo2> (<foo>) ())
(let ((f (create (class <foo2>) 'bar 2)))
  (print (get-bar f)))

;; check initialize-object
(defclass <foo3> () ((bar3 :reader get-bar3 :writer set-bar3 :initarg bar)))
(defmethod initialize-object ((f <foo3>) :rest args)
    (set-bar3 3 f)
    (call-next-method))
(print (get-bar3 (create (class <foo3>) 'bar 0)))

;; check accessor
(let ((obj (create (class <foo>))))
  (setf (bar obj) 4)
  (print (bar obj)))

;; check boundp
(let ((obj (create (class <foo>))))
  (if (bar-boundp obj)
      (print 0)
      (print 5))
  (setf (bar obj) 6)
  (if (bar-boundp obj)
      (print 6)
      (print 0)))
(finish-output (standard-output))
