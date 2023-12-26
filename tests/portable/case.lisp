(requires "testing.lisp")

;; test cases taken from spec

(test-equal
    (case (+ 2 4)
      ((2 3 5 7) 'prime)
      ((4 6 8 9) 'composite))
    'composite)

(test-equal
    (case (car '(c d))
      ((a) 'a)
      ((b) 'b))
    nil)

(test-equal
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((y) 'semivowel)
      (t 'consonant))
    'consonant)

(test-equal
(let ((char #\u))
  (case char
    ((#\a #\e #\o #\u #\i) 'vowels)
    (t 'consonants)))
  'vowels)

(test-equal
  (case-using #'= (+ 1.0 1.0)
    ((1) 'one)
    ((2) 'two)
    (t 'more))
  'two)

(test-equal
  (case-using #'string= "bar"
    (("foo") 1)
    (("bar") 2))
  2)

(format (standard-output) "case.lisp end")