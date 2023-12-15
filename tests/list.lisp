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

(test-equal (assoc 'a '((a . 1) (b . 2))) '(a . 1))
(test-equal (assoc 'a '((a . 1) (a . 2))) '(a . 1))
(test-equal (assoc 'c '((a . 1) (b . 2))) nil)

(test-equal
    (maplist (lambda (x) (cons 'foo x)) '(a b c d))
    '((foo a b c d) (foo b c d) (foo c d) (foo d)))

(format (standard-output) "list.lisp end")
