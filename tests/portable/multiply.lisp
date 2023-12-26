(requires "testing.lisp")

;; tests taken from spec

(test-equal
  (* 12 3)
  36)

(test-equal
  (* 12 3.0)
  36.0)

(test-equal
  (* 4.0 0)
  0.0)

(test-equal
  (* 2 3 4)
  24)

(test-equal
  (*)
  1)

(format (standard-output) "multiply.lisp end")