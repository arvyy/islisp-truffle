(defmacro test-equal (expr value)
  (let ((actual (gensym)))
    `(let ((,actual ,expr))
        (if (not (equal ,actual ,value))
          (progn
            (format-object (standard-output) "In expression: " nil)
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

(provides
    test-equal)