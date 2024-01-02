(requires "testing.lisp")

(test-equal (numberp 1) t)
(test-equal (numberp "1") nil)

(test-equal (= 1 (min 1)) t)
(test-equal (= 1 (min 3 1)) t)
(test-equal (= 1 (min 1 3)) t)

(test-equal (= 1 (max 1)) t)
(test-equal (= 3 (max 3 1)) t)
(test-equal (= 3 (max 1 3)) t)

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
