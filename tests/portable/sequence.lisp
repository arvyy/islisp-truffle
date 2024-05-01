(requires "testing.lisp")

;; other sequence ops tested in list.lisp, vector.lis, string.lisp.

(let ((a (list 1 2 3 4))
      (b (list 10 10 10 10)))
  (map-into a #'+ a b)
  (test-equal a (list 11 12 13 14))
  (test-equal b (list 10 10 10 10)))

(format (standard-output) "sequence.lisp end")
(finish-output (standard-output))
