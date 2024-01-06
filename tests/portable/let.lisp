(requires "testing.lisp")

(defun foo (a)
    (let ((a (+ 1 2))
          (b a))
      (+ a b)))
(test-equal
    (foo 0)
    3)

(defun bar (a)
    (let* ((a (+ 1 2))
           (a (+ 1 a))
           (b a))
      (+ a b)))

(test-equal
    (bar 0)
    8)

(test-equal (let ((foo 1)) foo) 1)

(defglobal *closure* (let ((foo 5)) (lambda () foo)))
(test-equal (funcall *closure*) 5)

(format (standard-output) "let.lisp end")
(finish-output (standard-output))
