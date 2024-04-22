(requires "testing.lisp")

;;reader
(test-equal #b10 2)
(test-equal #b-10 -2)
(test-equal #o10 8)
(test-equal #o-10 -8)
(test-equal #x1f 31)
(test-equal #x-1f -31)

;; type checks
(test-equal (numberp 1) t)
(test-equal (numberp "1") nil)

;; min and max
(test-equal (= 1 (min 1)) t)
(test-equal (= 1 (min 3 1)) t)
(test-equal (= 1 (min 1 3)) t)

(test-equal (= 1 (max 1)) t)
(test-equal (= 3 (max 3 1)) t)
(test-equal (= 3 (max 1 3)) t)

;; addition
(test-equal (= 3 (+ 1 2)) t)
(test-equal (= 3 (+ 1.0 2.0)) t)
(test-equal (= 10000000000000000001 (+ 10000000000000000000 1)) t)
(block exit
    (with-handler
      (lambda (condition)
        (test-equal
            (instancep condition (class <domain-error>))
            t)
        (return-from exit nil))
      (+ 1 "2")
      (test-equal nil t)))

;; subtraction
(test-equal (- 1) -1)
(test-equal (- -4.0) 4.0)
(test-equal (- 4.0) -4.0)
(test-equal (- 1 2) -1)
(test-equal (- 92 43) 49)
(test-equal (- 0.0 0.0) 0.0)
(test-equal (- 3 4 5) -6)

;; multiplication; tests taken from spec
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

;; division / reciprocal
(test-equal (reciprocal 2) 0.5)
(test-equal (quotient 10 5) 2)
(test-equal (quotient 1 2) 0.5)
(test-equal (quotient 2 -0.5) -4.0)
(test-equal (quotient 2 4 8) 0.0625)
(block exit
    (with-handler
      (lambda (condition)
        (test-equal
            (instancep condition (class <division-by-zero>))
            t)
        (return-from exit nil))
      (quotient 0.0 0.0)
      (test-equal nil t)))


;; trigonometry
(test-equal
    (and (> (sin 1) 0.841)
         (< (sin 1) 0.842))
    t)

(test-equal
    (and (> (cos 1) 0.540)
         (< (cos 1) 0.541))
    t)

(test-equal
    (and (> (tan 1) 1.557)
         (< (tan 1) 1.558))
    t)

(format-object (standard-output) "number.lisp end" nil)
(finish-output (standard-output))
