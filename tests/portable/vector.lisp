(requires "testing.lisp")

(let ((v1 (vector 1 2))
      (v2 (create-vector 3 3))
      (v3 #(4 5)))
  (test-equal (elt v1 0) 1)
  (test-equal (elt v2 0) 3)
  (test-equal (elt v3 0) 4))

;; sequence
(test-equal (length #(1 2)) 2)

(format-object (standard-output) "vector.lisp end" nil)
(finish-output (standard-output))
