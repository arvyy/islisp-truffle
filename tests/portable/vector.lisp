(requires "testing.lisp")

(let ((v1 (vector 1 2))
      (v2 (create-vector 3 3))
      (v3 #(4 5)))
  (test-equal (elt v1 0) 1)
  (test-equal (elt v2 0) 3)
  (test-equal (elt v3 0) 4))

;; sequence
(test-equal (length #(1 2)) 2)
(test-equal (elt #(1 2 3) 2) 3)
(let ((vec (vector 1 2 3)))
  (setf (elt vec 2) 4)
  (test-equal vec #(1 2 4)))

(format-object (standard-output) "vector.lisp end" nil)
(finish-output (standard-output))
