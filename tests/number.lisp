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

(test-equal (numberp 1) t)
(test-equal (numberp "1") nil)

(test-equal (= 1 (min 1)) t)
(test-equal (= 1 (min 3 1)) t)
(test-equal (= 1 (min 1 3)) t)

(test-equal (= 1 (max 1)) t)
(test-equal (= 3 (max 3 1)) t)
(test-equal (= 3 (max 1 3)) t)

(format-object (standard-output) "number.lisp end" nil)