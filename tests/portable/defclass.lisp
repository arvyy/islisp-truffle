(requires "testing.lisp")

(defclass <foo> ()
    ((bar :reader get-bar :writer set-bar :accessor bar :initarg bar :boundp bar-boundp)))

(let ((f (create (class <foo>) 'bar 1)))
  (test-equal (get-bar f) 1)
  (set-bar 2 f)
  (test-equal (get-bar f) 2))

;; check inheritance
(defclass <foo2> (<foo>) ())
(let ((f (create (class <foo2>) 'bar 2)))
  (test-equal (get-bar f) 2))

;; check initialize-object
(defclass <foo3> () ((bar3 :reader get-bar3 :writer set-bar3 :initarg bar)))
(defmethod initialize-object ((f <foo3>) :rest args)
    (set-bar3 3 f)
    (call-next-method))
(test-equal (get-bar3 (create (class <foo3>) 'bar 0)) 3)

;; check accessor
(let ((obj (create (class <foo>))))
  (setf (bar obj) 4)
  (test-equal (bar obj) 4))

;; check boundp
(let ((obj (create (class <foo>))))
  (test-equal (bar-boundp obj) nil)
  (setf (bar obj) 6)
  (test-equal (bar-boundp obj) t))

(format (standard-output) "defclass.lisp end")
(finish-output (standard-output))
