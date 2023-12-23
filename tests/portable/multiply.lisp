(defmacro test-equal (expr value)
  (let ((actual (gensym)))
    `(let ((,actual ,expr))
        (if (not (equal ,actual ,value))
          (progn
            (format-object (standard-output) ',expr t)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Expect: " nil)
            (format-object (standard-output) ,value t)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Actual: " nil)
            (format-object (standard-output) ,actual t)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "-------" nil)
            (format-char (standard-output) #\newline))))))

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