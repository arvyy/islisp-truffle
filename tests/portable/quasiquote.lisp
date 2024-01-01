(requires "testing.lisp")

(test-equal `(1 2) (list 1 2))
(test-equal `(1 ,2) (list 1 2))
(test-equal `(1 ,(+ 1 2)) (list 1 3))
(test-equal `#(1 ,(+ 1 2)) (vector 1 3))
(test-equal `#(1 ,@(progn '(2 3))) (vector 1 2 3))

;; nested quasiquote representation seems to be underspecified,
;; use a macro defining macro to portably test nested quasiquote behavior.
(defmacro m1 (a)
  `(defmacro m2 (b)
      `(list ,,a ,b)))

(m1 1)
(test-equal (m2 2) (list 1 2))

(format-object (standard-output) "quasiquote.lisp end" nil)
(finish-output (standard-output))
