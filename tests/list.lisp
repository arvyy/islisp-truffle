(defmacro test-equal (expr value)
  (let ((actual (gensym)))
    `(let ((,actual ,expr))
        (if (not (equal ,actual ,value))
          (progn
            (format-object (standard-output) ',expr t)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Expect: " t)
            (format-object (standard-output) ,value nil)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Actual: " t)
            (format-object (standard-output) ,actual nil)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "-------" nil)
            (format-char (standard-output) #\newline))))))

(test-equal (reverse (list 1 2 3)) (list 3 2 1))
(test-equal (nreverse (list 1 2 3)) (list 3 2 1))

;; taken from spec
(test-equal (assoc 'a '((a . 1) (b . 2))) '(a . 1))
(test-equal (assoc 'a '((a . 1) (a . 2))) '(a . 1))
(test-equal (assoc 'c '((a . 1) (b . 2))) nil)

;; taken from spec
(test-equal
  (member 'c '(a b c d e f))
  '(c d e f))
(test-equal
  (member 'g '(a b c d e f))
  nil)

;; taken from spec
(test-equal
    (maplist (lambda (x) (cons 'foo x)) '(a b c d))
    '((foo a b c d) (foo b c d) (foo c d) (foo d)))

;; taken from spec
(let ((k 0))
  (mapl (lambda (x)
          (setq k (+ k (if (member (car x) (cdr x)) 0 1))))
        '(a b a c d b c))
  (test-equal k 4))

;; taken from spec
(test-equal
  (mapcar #'car '((1 a) (2 b) (3 c)))
  '(1 2 3))

;; taken from spec
(let ((x 0))
  (mapc (lambda (v)
          (setq x (+ x v)))
        '(3 5))
  (test-equal x 8))

;; taken from spec
(test-equal
  (mapcan (lambda (x)
            (if (> x 0)
                (list x)))
          '(-3 4 0 5 -2 7))
  '(4 5 7))

;; taken from spec
(test-equal
  (mapcon (lambda (x)
            (if (member (car x) (cdr x))
                (list (car x))))
          '(a b a c d b c b c))
  '(a b c b c))

(format (standard-output) "list.lisp end")
