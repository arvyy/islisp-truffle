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

(test-equal
    (cond)
    nil)

(test-equal
    (cond ("1"))
    "1")

(test-equal
    (cond ((> 2 1) "1"))
    "1")

(test-equal
    (cond ((> 2 1) "1" "2"))
    "2")

(test-equal
    (cond
        ((< 2 1) "1")
        ((> 2 1) "2")
        ((> 3 1) "3"))
    "2")

(format (standard-output) "cond.lisp end")
